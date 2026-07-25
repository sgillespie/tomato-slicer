module System.Statusbar.Pomodoro.TimerTest (spec) where

import System.Statusbar.Pomodoro.Gen qualified as Gen
import System.Statusbar.Pomodoro.Timer
  ( CurrentTime (..),
    Duration (..),
    EndTime (..),
    Timer (..),
    formatDuration,
    remainingDuration,
    startTimer,
    tickTimer,
  )

import Data.Text qualified as Text
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Hedgehog (failure, forAll, (===))
import Hedgehog.Range qualified as Range
import System.Clock (TimeSpec, toNanoSecs)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = describe "System.Statusbar.Timer.Timer" $ do
  describe "startTimer" $ do
    it "sets expected deadline" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      duration@(Duration diffTime) <-
        forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundSecs)

      let timer = startTimer now duration
          expectedEndTime = timeSpecToDiffTime nowSpec + diffTime

      case timer of
        TimerRunning (EndTime endSpec) -> timeSpecToDiffTime endSpec === expectedEndTime
        TimerDone -> failure

  describe "tickTimer" $ do
    it "leaves an already-completed timer complete" $ hedgehog $ do
      now <- forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      tickTimer now TimerDone === TimerDone

    it "completes a timer when deadline is equal to current time" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      tickTimer now (TimerRunning (EndTime nowSpec)) === TimerDone

    it "leaves a running timer when deadline is greater than current time" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      endTime <-
        forAll $
          Gen.endTimeInNanos (Range.linear (toNanoSecs nowSpec + 1) Gen.upperBoundNanos)

      tickTimer now (TimerRunning endTime) === TimerRunning endTime

    it "completes a timer when deadline is less than current time" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 1 Gen.upperBoundNanos)
      endTime <-
        forAll $ Gen.endTimeInNanos (Range.linear 0 (toNanoSecs nowSpec - 1))

      tickTimer now (TimerRunning endTime) === TimerDone

  describe "remainingDuration" $ do
    it "returns 0 when timer is completed" $ hedgehog $ do
      now <- forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      remainingDuration now TimerDone === 0

    it "returns `end - now` with timer is running" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      endTime@(EndTime endSpec) <-
        forAll $
          Gen.endTimeInNanos (Range.linear (toNanoSecs nowSpec + 1) Gen.upperBoundNanos)

      let expectedDuration = Duration (timeSpecToDiffTime (endSpec - nowSpec))

      remainingDuration now (TimerRunning endTime) === expectedDuration

  describe "formatDuration" $ do
    it "has length of 5" $ hedgehog $ do
      duration <- forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundSecs)
      Text.length (formatDuration duration) === 5

    it "has a colon at index 2" $ hedgehog $ do
      duration <- forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundSecs)
      let indexOf str char = Text.findIndex (== char) str

      formatDuration duration `indexOf` ':' === Just 2

timeSpecToDiffTime :: TimeSpec -> DiffTime
timeSpecToDiffTime = picosecondsToDiffTime . (* 1000) . toNanoSecs
