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

import Control.Concurrent.STM (TBQueue, newTBQueueIO, retry, tryReadTBQueue)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.Default (Default (..))
import Data.Text.IO qualified as Text
import Data.Time (secondsToDiffTime)
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Clock (Clock (..), getTime)
import System.FilePath ((</>))
import System.Statusbar.Pomodoro.Error (ServerError (..))
import System.Statusbar.Pomodoro.Waybar (WaybarOutput (..), formatTimerState, timerTooltipText)
import System.XDG (getRuntimeDir)
import UnliftIO (MonadUnliftIO)
import UnliftIO qualified
import UnliftIO.Concurrent (forkFinally)

data ServerEnv = ServerEnv
  { commands :: TBQueue Command,
    durationInSeconds :: Word
  }

newtype Command = QueryStatus (TMVar Timer)

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
    UnliftIO.mapConcurrently_ id [runSock, runTimer]

runSock :: ServerT ()
runSock = do
  xdgRunDir <- liftIO getRuntimeDir
  let sockFile = xdgRunDir </> "tomato-slicer.socket"

  UnliftIO.bracket
    (liftIO $ openSock sockFile)
    (liftIO . Socket.close)
    (void . loopServer)
  where
    openSock :: FilePath -> IO Socket
    openSock sockFile =
      UnliftIO.bracketOnError mkSock Socket.close $ \sock -> do
        Socket.withFdSocket sock Socket.setCloseOnExecIfNeeded
        Socket.bind sock (Socket.SockAddrUnix sockFile)
        Socket.listen sock 1024

        Text.hPutStrLn stderr $ "Server listening on unix://" <> toText sockFile
        pure sock

    mkSock :: IO Socket
    mkSock = Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol

    loopServer :: Socket -> ServerT Void
    loopServer sock = do
      let accept' = liftIO . Socket.accept
          close' = liftIO . Socket.close
          gracefulClose' = liftIO . flip Socket.gracefulClose 5000

      infinitely $
        UnliftIO.bracketOnError (accept' sock) (close' . fst) $ \(conn, _) ->
          forkFinally (handleConn conn) (const $ gracefulClose' conn)

    handleConn :: Socket -> ServerT ()
    handleConn conn = do
      msg <- liftIO $ recv conn 1024

      unless (ByteString.null msg) $ do
        -- TODO[sgillespie]: Move this into a Katip logger
        liftIO . Text.hPutStrLn stderr $
          "Received request: '" <> decodeUtf8 msg <> "'"

        resp <-
          UnliftIO.catches
            (handleMsg msg)
            [ UnliftIO.Handler $ \(err :: ServerError) -> do
                liftIO . Text.hPutStrLn stderr $ "Could not handle request: " <> show err
                pure . toStrict . Aeson.encode $ handleErr err,
              UnliftIO.Handler $ \(_ :: SomeException) ->
                UnliftIO.throwIO ServerUnexpectedError
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
          Left err -> UnliftIO.throwIO $ JsonParseError (toText err)
          Right res -> Aeson.encode <$> handleReq res

      pure $ toStrict resp

handleReq :: ReqHandler ServerT () StatusResponse
handleReq Protocol.Req {reqCommand = Protocol.ReqStatus} = do
  now <- liftIO $ getTime Monotonic

  -- Write status query command to queue
  queue <- asks commands
  var <- atomically $ do
    var <- newEmptyTMVar
    UnliftIO.writeTBQueue queue (QueryStatus var)
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
      ServerEnv {..} <- ask
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

    untilDelay 1_000_000 $ runMaybeT $ do
      (QueryStatus var) <- MaybeT $ tryReadTBQueue commands
      lift . void $ tryPutTMVar var timer

-- | Repeatedly run an STM action until the delay has expired.
--
-- DO NOT run any blocking STM actions, as they may continue to block after the delay
-- has passed. Instead, use the `tryX` class of non-blocking STM actions. This function
-- handles the result and retries if necessary.
untilDelay :: (MonadIO io) => Int -> STM (Maybe a) -> io ()
untilDelay delay action = do
  -- Store the delay expiry state
  timeout <- UnliftIO.registerDelay delay

  expired <- atomically $ do
    -- Run the STM action
    action >>= \case
      -- It was successful, commit the transaction and return the expiry state
      Just _ -> readTVar timeout
      -- It was unsuccessful; If the delay has expired, commit the transaction, otherwise
      -- retry.
      Nothing ->
        readTVar timeout >>= \case
          -- Timeout has expired--commit the transaction with expired state
          True -> pure True
          False -> retry

  -- If the timeout expired, exit; otherwise, loop again.
  unless expired $
    untilDelay delay action

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
