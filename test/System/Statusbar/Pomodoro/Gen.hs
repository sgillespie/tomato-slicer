module System.Statusbar.Pomodoro.Gen
  ( -- * Generators
    durationInSecs,
    diffTimeInSecs,
    currentTimeInNanos,
    endTimeInNanos,
    timeSpecInNanos,
    remainingTimeInNanos,

    -- * Ranges
    upperBoundSecs,
    upperBoundNanos,
  ) where

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

timeSpecInNanos :: Range Integer -> Gen TimeSpec
timeSpecInNanos range = fromNanoSecs <$> Gen.integral range

upperBoundNanos :: Integer
upperBoundNanos = 1_000_000_000_000
