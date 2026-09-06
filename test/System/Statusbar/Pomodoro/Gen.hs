module System.Statusbar.Pomodoro.Gen
  ( -- * Generators

    -- ** Timer
    durationInSecs,
    diffTimeInSecs,
    currentTimeInNanos,
    endTimeInNanos,
    timeSpecInNanos,
    remainingTimeInNanos,

    -- ** Protocol
    protoVersion,
    requestId,
    req,
    reqCommand,
    resp,
    respStatus,
    errorResponse,
    statusResponse,
    respTimerState,

    -- * Ranges
    upperBoundSecs,
    upperBoundNanos,
  ) where

import System.Statusbar.Pomodoro.Protocol
  ( ErrorResponse (..),
    ProtoVersion (..),
    Req,
    ReqCommand (..),
    RequestId (..),
    Resp,
    RespStatus (..),
    RespTimerState (..),
    StatusResponse (..),
  )
import System.Statusbar.Pomodoro.Protocol qualified as Protocol
import System.Statusbar.Pomodoro.Timer
  ( CurrentTime (..),
    Duration (..),
    EndTime (..),
    RemainingTime (..),
  )

import Data.Time (DiffTime, secondsToDiffTime)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as Range
import System.Clock (TimeSpec, fromNanoSecs)

durationInSecs :: Range Integer -> Gen Duration
durationInSecs = fmap Duration . diffTimeInSecs

diffTimeInSecs :: Range Integer -> Gen DiffTime
diffTimeInSecs range = secondsToDiffTime <$> Gen.integral range

upperBoundSecs :: Integer
upperBoundSecs = 3600

currentTimeInNanos :: Range Integer -> Gen CurrentTime
currentTimeInNanos = fmap CurrentTime . timeSpecInNanos

endTimeInNanos :: Range Integer -> Gen EndTime
endTimeInNanos = fmap EndTime . timeSpecInNanos

remainingTimeInNanos :: Range Integer -> Gen RemainingTime
remainingTimeInNanos = fmap RemainingTime . timeSpecInNanos

protoVersion :: Gen ProtoVersion
protoVersion = pure (ProtoVersion 1)

requestId :: Gen RequestId
requestId = RequestId <$> Gen.word (Range.linear minBound maxBound)

req :: Gen (Req a)
req = do
  ver <- protoVersion
  reqId <- requestId
  cmd <- reqCommand

  pure $
    Protocol.Req
      { reqVersion = ver,
        reqId = reqId,
        reqCommand = cmd
      }

reqCommand :: Gen ReqCommand
reqCommand = pure ReqStatus

resp :: Gen a -> Gen (Resp a)
resp extra = do
  ver <- protoVersion
  respId <- Gen.maybe requestId
  status <- respStatus
  extra' <- extra

  pure $
    Protocol.Resp
      { respVersion = ver,
        respId = respId,
        respStatus = status,
        respData = extra'
      }

respStatus :: Gen RespStatus
respStatus = Gen.element [Protocol.OK, Protocol.Error]

errorResponse :: Gen ErrorResponse
errorResponse =
  ErrorResponse <$> Gen.text (Range.linear min' max') Gen.unicode
  where
    min' = 0 -- Can't have negative length strings
    max' = 1000 -- Reasonably low to keep tests fast

statusResponse :: Range Integer -> Gen StatusResponse
statusResponse durationRange = do
  state' <- respTimerState
  duration' <- durationInSecs durationRange

  pure $
    Protocol.StatusResponse
      { statusRespState = state',
        statusRespTime = duration'
      }

respTimerState :: Gen RespTimerState
respTimerState =
  Gen.element
    [ RespStateReady,
      RespStateDone,
      RespStateRunning,
      RespStatePaused
    ]

timeSpecInNanos :: Range Integer -> Gen TimeSpec
timeSpecInNanos range = fromNanoSecs <$> Gen.integral range

upperBoundNanos :: Integer
upperBoundNanos = 1_000_000_000_000
