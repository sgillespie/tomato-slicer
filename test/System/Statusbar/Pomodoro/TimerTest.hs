module System.Statusbar.Pomodoro.TimerTest (spec) where

import System.Statusbar.Pomodoro.Gen qualified as Gen
import System.Statusbar.Pomodoro.Timer
  ( CurrentTime (..),
    Duration (..),
    EndTime (..),
    RemainingTime (..),
    Timer (..),
    diffTimeToTimeSpec,
    formatDuration,
    pauseTimer,
    remainingDuration,
    resumeTimer,
    startTimer,
    tickTimer,
    togglePausedTimer,
  )

import Data.Text qualified as Text
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Hedgehog (annotateShow, failure, forAll, tripping, (===))
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
        TimerReady -> failure
        TimerDone -> failure
        TimerPaused {} -> failure

  describe "tickTimer" $ do
    it "leaves a non-running timer non-running" $ hedgehog $ do
      now <- forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      tickTimer now TimerDone === TimerDone
      tickTimer now TimerReady === TimerReady

      remaining <- forAll $ Gen.remainingTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      tickTimer now (TimerPaused remaining) === TimerPaused remaining

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

  describe "pauseTimer" $ do
    it "leaves a non-running timer non-running" $ hedgehog $ do
      now <- forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      pauseTimer now TimerDone === TimerDone
      pauseTimer now TimerReady === TimerReady

      remaining <- forAll $ Gen.remainingTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      pauseTimer now (TimerPaused remaining) === TimerPaused remaining

    it "sets a running timer's state to paused" $ hedgehog $ do
      now@(CurrentTime nowSpec) <- forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      end@(EndTime endSpec) <- forAll $ Gen.endTimeInNanos (Range.linear (toNanoSecs nowSpec + 1) Gen.upperBoundNanos)

      let res = pauseTimer now (TimerRunning end)
      annotateShow res

      case res of
        TimerPaused (RemainingTime remaining) -> remaining === endSpec - nowSpec
        _ -> failure

  describe "resumeTimer" $ do
    it "leaves a non-paused timer non-paused" $ hedgehog $ do
      now <- forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      end <- forAll $ Gen.endTimeInNanos (Range.linear 0 Gen.upperBoundNanos)

      resumeTimer now TimerReady === TimerReady
      resumeTimer now TimerDone === TimerDone
      resumeTimer now (TimerRunning end) === TimerRunning end

    it "sets a paused timer's state to running" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      remaining@(RemainingTime remainingSpec) <-
        forAll $ Gen.remainingTimeInNanos (Range.linear 0 Gen.upperBoundNanos)

      let expectedEnd = EndTime $ nowSpec + remainingSpec

      resumeTimer now (TimerPaused remaining) === TimerRunning expectedEnd

  describe "togglePausedTimer" $ do
    it "leaves non-paused/non-running timer non-paused/non-running" $ hedgehog $ do
      now <- forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      togglePausedTimer now TimerReady === TimerReady
      togglePausedTimer now TimerDone === TimerDone

    it "sets paused timer's state to running" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      end@(EndTime endSpec) <-
        forAll $
          Gen.endTimeInNanos (Range.linear (toNanoSecs nowSpec + 1) Gen.upperBoundNanos)

      let expectedRemaining = RemainingTime (endSpec - nowSpec)

      togglePausedTimer now (TimerRunning end) === TimerPaused expectedRemaining

    it "sets a running timer's state to paused" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      remaining@(RemainingTime remainingSpec) <-
        forAll $
          Gen.remainingTimeInNanos $
            Range.linear (toNanoSecs nowSpec + 1) Gen.upperBoundNanos

      let expectedEnd = EndTime (nowSpec + remainingSpec)

      togglePausedTimer now (TimerPaused remaining) === TimerRunning expectedEnd

    it "round trips a running timer" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      end <-
        forAll $
          Gen.endTimeInNanos (Range.linear (toNanoSecs nowSpec + 1) Gen.upperBoundNanos)

      tripping (TimerRunning end) (togglePausedTimer now) (Identity . togglePausedTimer now)

    it "round trips a paused timer" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      remaining <-
        forAll $
          Gen.remainingTimeInNanos $
            Range.linear (toNanoSecs nowSpec + 1) Gen.upperBoundNanos

      tripping
        (TimerPaused remaining)
        (togglePausedTimer now)
        (Identity . togglePausedTimer now)

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

  describe "diffTimeToTimeSpec" $ do
    it "round-trips through timeSpecToDiffTime" $ hedgehog $ do
      diffTime <- forAll $ Gen.diffTimeInSecs (Range.linear 0 Gen.upperBoundSecs)
      tripping diffTime diffTimeToTimeSpec (Identity . timeSpecToDiffTime)

  describe "timeSpecToDiffTime" $ do
    it "rount-trips though diffTimeToTimeSpec" $ hedgehog $ do
      timeSpec <- forAll $ Gen.timeSpecInNanos (Range.linear 0 Gen.upperBoundNanos)
      tripping timeSpec timeSpecToDiffTime (Identity . diffTimeToTimeSpec)

timeSpecToDiffTime :: TimeSpec -> DiffTime
timeSpecToDiffTime = picosecondsToDiffTime . (* 1000) . toNanoSecs
