module System.Statusbar.Pomodoro.Gen
  ( durationInSecs,
    currentTimeInNanos,
    endTimeInNanos,
    upperBoundSecs,
    upperBoundNanos,
  ) where

import System.Statusbar.Pomodoro.Timer (CurrentTime (..), Duration (..), EndTime (..))

import Data.Time (secondsToDiffTime)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (Range)
import System.Clock (fromNanoSecs)

durationInSecs :: Range Integer -> Gen Duration
durationInSecs range = Duration . secondsToDiffTime <$> Gen.integral range

upperBoundSecs :: Integer
upperBoundSecs = 3600

currentTimeInNanos :: Range Integer -> Gen CurrentTime
currentTimeInNanos range = CurrentTime . fromNanoSecs <$> Gen.integral range

endTimeInNanos :: Range Integer -> Gen EndTime
endTimeInNanos range = EndTime . fromNanoSecs <$> Gen.integral range

upperBoundNanos :: Integer
upperBoundNanos = 1_000_000_000_000
