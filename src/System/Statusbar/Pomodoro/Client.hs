module System.Statusbar.Pomodoro.Client
  ( runStatus,
  ) where

import System.Statusbar.Pomodoro.Protocol qualified as Protocol

import Data.Aeson qualified as Aeson
import Data.Text.IO qualified as Text
import Network.Socket qualified as Network
import Network.Socket.ByteString (recv, sendAll)
import System.FilePath ((</>))
import System.XDG (getRuntimeDir)
import UnliftIO (bracketOnError)

runStatus :: IO ()
runStatus = do
  xdgRunDir <- getRuntimeDir
  let sockFile = xdgRunDir </> "tomato-slicer.socket"
      mkSocket = Network.socket Network.AF_UNIX Network.Stream Network.defaultProtocol

  bracketOnError mkSocket Network.close $ \sock -> do
    Network.connect sock (Network.SockAddrUnix sockFile)
    sendAll sock $
      toStrict $
        Aeson.encode $
          Protocol.Req
            { reqVersion = Protocol.ProtoVersion 1,
              reqId = Protocol.RequestId 1,
              reqCommand = Protocol.ReqStatus
            }
    msg <- recv sock 1024
    Text.hPutStrLn stderr $ "Received msg: '" <> decodeUtf8 msg <> "'"
