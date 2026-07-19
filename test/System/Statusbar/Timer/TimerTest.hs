module System.Statusbar.Timer.TimerTest (spec) where

import System.Statusbar.Timer.Gen (genCurrentTime, genDuration)
import System.Statusbar.Timer.Timer

import Data.Time.Clock (diffTimeToPicoseconds)
import Hedgehog
import System.Clock (fromNanoSecs)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = describe "System.Statusbar.Timer.Timer" $ do
  it "startTimer sets expected deadline" $ hedgehog $ do
    now@(CurrentTime nowSpec) <- forAll genCurrentTime
    duration@(Duration diffTime) <- forAll genDuration

    let timer = startTimer now duration
        endSpec = fromNanoSecs $ diffTimeToPicoseconds diffTime `div` 1000
        expectedEnd = EndTime (nowSpec + endSpec)

    case timer of
      TimerRunning end -> end === expectedEnd
      TimerDone -> failure
