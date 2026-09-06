{-# LANGUAGE TypeFamilies #-}

module System.Statusbar.Pomodoro.Server
  ( ServerEnv (..),
    ServerT (..),
    runServerT,
    runServer,
    runSock,
  ) where

import System.Statusbar.Pomodoro.Protocol
  ( ErrorResponse,
    Req,
    ReqHandler,
    Resp,
    RespTimerState,
    StatusResponse,
  )
import System.Statusbar.Pomodoro.Protocol qualified as Protocol
import System.Statusbar.Pomodoro.Timer (CurrentTime, Duration, Timer)
import System.Statusbar.Pomodoro.Timer qualified as Timer

import Control.Concurrent.STM (TBQueue, newTBQueueIO)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.Default (Default (..))
import Data.Text.IO qualified as Text
import Data.Time (secondsToDiffTime)
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..), accept, bind, close, defaultProtocol, gracefulClose, listen, setCloseOnExecIfNeeded, socket, withFdSocket)
import Network.Socket.ByteString (recv, sendAll)
import System.Clock (Clock (..), getTime)
import System.FilePath ((</>))
import System.Statusbar.Pomodoro.Error (ServerError (..))
import System.Statusbar.Pomodoro.Waybar (WaybarOutput (..), formatTimerState, timerTooltipText)
import System.XDG (getRuntimeDir)
import UnliftIO (Handler (..), MonadUnliftIO (..), bracket, bracketOnError, catches, flushTBQueue, mapConcurrently_, throwIO, writeTBQueue)
import UnliftIO.Concurrent (forkFinally, threadDelay)

data ServerEnv = ServerEnv
  { commands :: TBQueue Command,
    durationInSeconds :: Word
  }

data Command
  = QueryStatus (TMVar Timer)

newtype ServerT a = ServerT {unServerT :: ReaderT ServerEnv IO a}
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadReader ServerEnv
    )

runServerT :: ServerEnv -> ServerT a -> IO a
runServerT env action = runReaderT (unServerT action) env

runServer :: Word -> IO ()
runServer durationInSeconds = do
  queue <- newTBQueueIO 10
  let env =
        ServerEnv
          { commands = queue,
            durationInSeconds = durationInSeconds
          }

  runServerT env $
    mapConcurrently_ id [runSock, runTimer]

runSock :: ServerT ()
runSock = do
  xdgRunDir <- liftIO getRuntimeDir
  let sockFile = xdgRunDir </> "tomato-slicer.socket"

  bracket
    (liftIO $ openSock sockFile)
    (liftIO . close)
    (void . loopServer)
  where
    openSock sockFile =
      bracketOnError (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock (SockAddrUnix sockFile)
        listen sock 1024

        Text.hPutStrLn stderr $ "Server listening on unix://" <> toText sockFile
        pure sock

    loopServer :: Socket -> ServerT Void
    loopServer sock = do
      infinitely $
        bracketOnError (liftIO $ accept sock) (liftIO . close . fst) $ \(conn, _) ->
          forkFinally (handleConn conn) (const $ liftIO $ gracefulClose conn 5000)

    handleConn :: Socket -> ServerT ()
    handleConn conn = do
      msg <- liftIO $ recv conn 1024

      unless (ByteString.null msg) $ do
        -- TODO[sgillespie]: Move this into a Katip logger
        liftIO . Text.hPutStrLn stderr $
          "Received request: '" <> decodeUtf8 msg <> "'"

        resp <-
          handleMsg msg
            `catches` [ Handler $ \(err :: ServerError) -> do
                          liftIO $ putStrLn $ "Could not handle request: " <> show err
                          pure $ toStrict $ Aeson.encode $ handleErr err,
                        Handler $ \(_ :: SomeException) ->
                          throwIO ServerUnexpectedError
                      ]

        liftIO $ Text.hPutStrLn stderr ("Sending response: '" <> decodeUtf8 resp <> "'")
        liftIO $ sendAll conn resp

        handleConn conn

    handleMsg :: ByteString -> ServerT ByteString
    handleMsg reqStr = do
      -- Parse request
      let parsed = Aeson.eitherDecodeStrict @(Req ()) reqStr
      -- If successful, continue. Otherwise, throw an error and return to the server loop
      resp <-
        case parsed of
          Left err -> throwIO $ JsonParseError (toText err)
          Right res -> Aeson.encode <$> handleReq res

      pure $ toStrict resp

handleReq :: ReqHandler ServerT () StatusResponse
handleReq Protocol.Req {reqCommand = Protocol.ReqStatus} = do
  now <- liftIO $ getTime Monotonic

  -- Write status query command to queue
  queue <- asks commands
  var <- atomically $ do
    var <- newEmptyTMVar
    writeTBQueue queue (QueryStatus var)
    pure var
  -- Wait for the result
  timer <- atomically $ readTMVar var
  -- Transform it into the response format
  statusResp <- toStatusResponse (Timer.CurrentTime now) timer

  pure $
    Protocol.Resp
      { respVersion = Protocol.ProtoVersion 1,
        respId = Nothing,
        respStatus = Protocol.OK,
        respData = statusResp
      }
  where
    toStatusResponse :: CurrentTime -> Timer -> ServerT StatusResponse
    toStatusResponse now timer = do
      ServerEnv{..} <- ask
      let duration = Timer.Duration $ secondsToDiffTime (fromIntegral durationInSeconds)

      pure $
        Protocol.StatusResponse
          { statusRespState = toRespTimerState timer,
            statusRespTime = Timer.remainingDuration duration now timer
          }

    toRespTimerState :: Timer -> RespTimerState
    toRespTimerState Timer.TimerReady = Protocol.RespStateReady
    toRespTimerState Timer.TimerDone = Protocol.RespStateDone
    toRespTimerState (Timer.TimerRunning _) = Protocol.RespStateRunning
    toRespTimerState (Timer.TimerPaused _) = Protocol.RespStatePaused

handleErr :: ServerError -> Resp ErrorResponse
handleErr err =
  Protocol.Resp
    { respVersion = Protocol.ProtoVersion 1,
      respId = Nothing,
      respStatus = Protocol.Error,
      respData =
        Protocol.ErrorResponse
          { errRespMsg = show err
          }
    }

runTimer :: ServerT ()
runTimer = do
  ServerEnv {..} <- ask

  let barOut = def
      duration = Timer.Duration $ secondsToDiffTime (fromIntegral durationInSeconds)
  timerRef <- newIORef Timer.newTimer

  void . infinitely $ do
    -- Update timer, print state
    now <- liftIO $ Timer.CurrentTime <$> getTime Monotonic
    timer <- updateTimer timerRef (Timer.tickTimer now)
    printTimerState barOut duration now timer

    -- TODO[sgillespie]: Handle messages asynchronously so it can respond within 100ms
    -- Handle queued message
    atomically $ do
      cmds <- flushTBQueue commands
      forM_ cmds $ \(QueryStatus var) -> tryPutTMVar var timer

    threadDelay 1_000_000

updateTimer :: (MonadIO io) => IORef Timer -> (Timer -> Timer) -> io Timer
updateTimer timerRef advance = do
  timer <- readIORef timerRef
  writeIORef timerRef (advance timer)
  readIORef timerRef

printTimerState :: (MonadIO io) => WaybarOutput -> Duration -> CurrentTime -> Timer -> io ()
printTimerState barOut duration now timer = putLBSLn . Aeson.encode $ barOut'
  where
    barOut' =
      barOut
        { wcoText = formatTimerState duration now timer,
          wcoAlt = Just (Timer.timerStateText timer),
          wcoClass = Just (Timer.timerStateText timer),
          wcoTooltip = Just (timerTooltipText duration now timer),
          wcoPercentage = Just $ percentage duration now timer
        }

percentage :: Duration -> CurrentTime -> Timer -> Word
percentage duration now timer =
  floor @Rational $ 100 * (1 - remaining / duration')
  where
    remaining = realToFrac $ Timer.getDuration (Timer.remainingDuration duration now timer)
    duration' = realToFrac (Timer.getDuration duration)

-- 1. Launch async threads
--    - Server (synchronous)
--    - Timer loop
--    - TBQueue (commands)
