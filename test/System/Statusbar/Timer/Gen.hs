module System.Statusbar.Timer.Gen
  ( genDuration,
    genCurrentTime,
  ) where

import System.Statusbar.Timer.Timer (Duration(..), CurrentTime(..))

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Data.Time (secondsToDiffTime)
import System.Clock (fromNanoSecs)

genDuration :: Gen Duration
genDuration = Duration . secondsToDiffTime <$> Gen.integral (Range.linear 1 3600)

genCurrentTime :: Gen CurrentTime
genCurrentTime = 
  CurrentTime . fromNanoSecs <$> Gen.integral (Range.linear 0 1000000000000)

