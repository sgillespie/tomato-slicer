module System.Statusbar.Timer.TimerTest (spec) where

import System.Statusbar.Timer.Gen (genDuration, genCurrentTime)
import System.Statusbar.Timer.Timer

import Hedgehog
import System.Clock (fromNanoSecs)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Data.Time.Clock (diffTimeToPicoseconds)

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
