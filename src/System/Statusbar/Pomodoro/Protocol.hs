module System.Statusbar.Pomodoro.Protocol
  ( ProtoVersion (..),
    RequestId (..),
    Req (..),
    ReqCommand (..),
    Resp (..),
    RespStatus (..),
    ErrorResponse (..),
    StatusResponse (..),
    RespTimerState (..),
    ReqHandler,
  ) where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import System.Statusbar.Pomodoro.Timer (RemainingTime)

-- | Protocol version ('1'), used to detect incompatible Server/Client interactions
newtype ProtoVersion = ProtoVersion {unProtoVersion :: Word}
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Num, FromJSON, ToJSON)

-- | A correlation ID between a request and response
newtype RequestId = RequestId {unRequestId :: Word}
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Num, FromJSON, ToJSON)

-- | A synchronous request message
data Req ext = Req
  { -- | Protocol version: set to '1'
    reqVersion :: ProtoVersion,
    -- | Client-generated correlation ID
    reqId :: RequestId,
    -- | The RPC request command
    reqCommand :: ReqCommand
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToJSON, FromJSON)

-- | An RPC request command
data ReqCommand
  = -- | Timer state
    ReqStatus
  deriving stock (Eq, Enum, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | A response to a synchronous request message
data Resp ext = Resp
  { -- | Protocol version: must match 'Req.reqVersion'
    respVersion :: ProtoVersion,
    -- | Client-generated correlation ID, if available
    respId :: Maybe (RequestId),
    -- | Response status indicating overall success or error
    respStatus :: RespStatus,
    respData :: ext
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Status of the response, can either be a success or error
data RespStatus 
  = OK    -- ^ Successful response
  | Error -- ^ Error result
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToJSON, FromJSON)

-- | An error message, encoded in the 'respData' field of 'Resp'. Implies
-- 'Resp.respStatus' is 'Error'
newtype ErrorResponse = ErrorResponse
  { errRespMsg :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

-- | Response data field from a 'ReqStatus' request command
data StatusResponse = StatusResponse
  { -- | The timer running state
    statusRespState :: RespTimerState,
    -- | The amount of time running on the currently running timer
    statusRespTime :: RemainingTime
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Enumeration of possible timer states, encoded in 'statusRespState' field of
-- 'StatusResponse'
data RespTimerState
  -- | Stopped and ready to be started
  = RespStateReady
  -- | Expired
  | RespStateDone
  -- | Currently running
  | RespStateRunning
  -- | Running but paused
  | RespStatePaused
  deriving stock (Eq, Enum, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | A request handler
type ReqHandler req resp = Req req -> Resp resp
