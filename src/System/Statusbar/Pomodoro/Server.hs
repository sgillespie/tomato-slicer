{-# LANGUAGE TypeFamilies #-}

module System.Statusbar.Pomodoro.Server
  ( ServerEnv (..),
    ServerT (..),
    runServerT,
    runServer,
    runSock,
  ) where

import System.Statusbar.Pomodoro.Protocol (ErrorResponse, Req (..), ReqHandler, Resp (..), StatusResponse (..))
import System.Statusbar.Pomodoro.Protocol qualified as Protocol
import System.Statusbar.Pomodoro.Timer (RemainingTime (..))

import Control.Concurrent.STM (TBQueue, newTBQueueIO)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Network.Socket (Family (..), SockAddr (..), Socket, SocketType (..), accept, bind, close, defaultProtocol, gracefulClose, listen, setCloseOnExecIfNeeded, socket, withFdSocket)
import Network.Socket.ByteString (recv, sendAll)
import System.FilePath ((</>))
import System.IO.Error (userError)
import System.XDG (getRuntimeDir)
import UnliftIO (MonadUnliftIO (..), bracket, bracketOnError, mapConcurrently_, throwIO, try)
import UnliftIO.Concurrent (forkFinally)

data ServerEnv = ServerEnv
  { commands :: TBQueue (),
    durationInSeconds :: Word
  }

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
    loopServer
  where
    openSock sockFile =
      bracketOnError (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock (SockAddrUnix sockFile)
        listen sock 1024
        pure sock

    loopServer :: Socket -> ServerT a
    loopServer sock = do
      forever $
        bracketOnError (liftIO $ accept sock) (liftIO . close . fst) $ \(conn, _) ->
          forkFinally (handleConn conn) (const $ liftIO $ gracefulClose conn 5000)

    handleConn :: Socket -> ServerT ()
    handleConn conn = do
      msg <- liftIO $ recv conn 1024

      putStrLn $ "Received request: " <> show msg

      unless (ByteString.null msg) $ do
        resp <- try (handleMsg msg)
        case resp of
          Left (err :: SomeException) -> do
            putStrLn $ "Could not handle request: " <> show err
            liftIO $ sendAll conn . toStrict . Aeson.encode $ handleErr err
          Right res -> do
            putStrLn $ "Sending response: " <> show resp
            liftIO $ sendAll conn res

        handleConn conn

    handleMsg :: ByteString -> ServerT ByteString
    handleMsg reqStr = do
      -- Parse request
      let parsed = Aeson.eitherDecodeStrict @(Req ()) reqStr
      -- If successful, continue. Otherwise, throw an error and return to the server loop
      resp <-
        case parsed of
          Left err -> do
            putStrLn $ "Cannot parse message: " <> err
            throwIO $ userError err
          Right res -> pure $ Aeson.encode (handleReq res)

      pure $ toStrict resp

handleReq :: ReqHandler () StatusResponse
handleReq Req {reqCommand = Protocol.ReqStatus} =
  Resp
    { respVersion = Protocol.ProtoVersion 1,
      respId = Nothing,
      respStatus = Protocol.OK,
      respData =
        StatusResponse
          { statusRespState = Protocol.RespStateReady,
            statusRespTime = RemainingTime 0
          }
    }

handleErr :: (Exception e) => e -> Resp ErrorResponse
handleErr err =
  Resp
    { respVersion = Protocol.ProtoVersion 1,
      respId = Nothing,
      respStatus = Protocol.Error,
      respData =
        Protocol.ErrorResponse
          { errRespMsg = show err
          }
    }

runTimer :: ServerT ()
runTimer = pure ()

-- 1. Launch async threads
--    - Server (synchronous)
--    - Timer loop
--    - TBQueue (commands)
