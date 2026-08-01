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
    newTimer,
    pauseTimer,
    remainingDuration,
    resetTimer,
    resumeTimer,
    startTimer,
    tickTimer,
    timerStateText,
    toggleRunningTimer,
  )

import Data.Text qualified as Text
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Hedgehog (annotateShow, failure, forAll, tripping, (===))
import Hedgehog.Range qualified as Range
import System.Clock (TimeSpec, toNanoSecs)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = describe "System.Statusbar.Timer.Timer" $ do
  describe "newTimer" $ do
    it "sets state to ready" $
      newTimer `shouldBe` TimerReady

  describe "startTimer" $ do
    it "leaves a non-ready timer non-ready" $ hedgehog $ do
      let rangeNanos = Range.linear 0 Gen.upperBoundNanos
          rangeSecs = Range.linear 0 Gen.upperBoundSecs

      now <- forAll $ Gen.currentTimeInNanos rangeNanos
      duration <- forAll $ Gen.durationInSecs rangeSecs
      endTime <- forAll $ Gen.endTimeInNanos rangeNanos
      remainingTime <- forAll $ Gen.remainingTimeInNanos rangeNanos

      startTimer duration now TimerDone === TimerDone
      startTimer duration now (TimerRunning endTime) === TimerRunning endTime
      startTimer duration now (TimerPaused remainingTime) === TimerPaused remainingTime

    it "starts a ready timer" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      duration@(Duration diffTime) <-
        forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundSecs)

      let expectedEndTime = EndTime (nowSpec + diffTimeToTimeSpec diffTime)

      startTimer duration now TimerReady === TimerRunning expectedEndTime

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

  describe "toggleRunningTimer" $ do
    it "leaves a done timer done" $ hedgehog $ do
      now <- forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      duration <- forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundSecs)

      toggleRunningTimer duration now TimerDone === TimerDone

    it "starts a ready timer" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      duration@(Duration diffTime) <-
        forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundNanos)

      let expectedEndTime = diffTimeToTimeSpec diffTime + nowSpec

      toggleRunningTimer duration now TimerReady === TimerRunning (EndTime expectedEndTime)

    it "sets paused timer's state to running" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      end@(EndTime endSpec) <-
        forAll $
          Gen.endTimeInNanos (Range.linear (toNanoSecs nowSpec + 1) Gen.upperBoundNanos)
      duration <- forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundSecs)

      let expectedRemaining = RemainingTime (endSpec - nowSpec)

      toggleRunningTimer duration now (TimerRunning end) === TimerPaused expectedRemaining

    it "sets a running timer's state to paused" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      remaining@(RemainingTime remainingSpec) <-
        forAll $
          Gen.remainingTimeInNanos $
            Range.linear (toNanoSecs nowSpec + 1) Gen.upperBoundNanos
      duration <- forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundSecs)

      let expectedEnd = EndTime (nowSpec + remainingSpec)

      toggleRunningTimer duration now (TimerPaused remaining) === TimerRunning expectedEnd

    it "round trips a running timer" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      end <-
        forAll $
          Gen.endTimeInNanos (Range.linear (toNanoSecs nowSpec + 1) Gen.upperBoundNanos)
      duration <- forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundSecs)

      tripping
        (TimerRunning end)
        (toggleRunningTimer duration now)
        (Identity . toggleRunningTimer duration now)

    it "round trips a paused timer" $ hedgehog $ do
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      remaining <-
        forAll $
          Gen.remainingTimeInNanos $
            Range.linear (toNanoSecs nowSpec + 1) Gen.upperBoundNanos
      duration <- forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundSecs)

      tripping
        (TimerPaused remaining)
        (toggleRunningTimer duration now)
        (Identity . toggleRunningTimer duration now)

  describe "resetTimer" $ do
    it "resets a ready timer" $ hedgehog $ do
      resetTimer TimerReady === TimerReady

    it "resets a completed timer" $ hedgehog $ do
      resetTimer TimerDone === TimerReady

    it "resets a running timer" $ hedgehog $ do
      end <- forAll $ Gen.endTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      resetTimer (TimerRunning end) === TimerReady

    it "resets a paused timer" $ hedgehog $ do
      remaining <-
        forAll $ Gen.remainingTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      resetTimer (TimerPaused remaining) === TimerReady

  describe "timerStateText" $ do
    it "returns expected text" $ hedgehog $ do
      endTime <- forAll $ Gen.endTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      remainingTime <-
        forAll $ Gen.remainingTimeInNanos (Range.linear 0 Gen.upperBoundNanos)

      timerStateText TimerReady === "ready"
      timerStateText TimerDone === "done"
      timerStateText (TimerRunning endTime) === "running"
      timerStateText (TimerPaused remainingTime) === "paused"

  describe "remainingDuration" $ do
    it "returns duration when timer is ready" $ hedgehog $ do
      duration <- forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundSecs)
      now <- forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)

      remainingDuration duration now TimerReady === duration

    it "returns 0 when timer is completed" $ hedgehog $ do
      duration <- forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundSecs)
      now <- forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)

      remainingDuration duration now TimerDone === 0

    it "returns remaining time when timer is paused" $ hedgehog $ do
      duration <- forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundSecs)
      now <- forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      remaining@(RemainingTime remainingSpec) <-
        forAll $ Gen.remainingTimeInNanos (Range.linear 0 Gen.upperBoundNanos)

      let getDuration = Duration . timeSpecToDiffTime

      remainingDuration duration now (TimerPaused remaining) === getDuration remainingSpec

    it "returns `end - now` with timer is running" $ hedgehog $ do
      duration <- forAll $ Gen.durationInSecs (Range.linear 0 Gen.upperBoundSecs)
      now@(CurrentTime nowSpec) <-
        forAll $ Gen.currentTimeInNanos (Range.linear 0 Gen.upperBoundNanos)
      endTime@(EndTime endSpec) <-
        forAll $
          Gen.endTimeInNanos (Range.linear (toNanoSecs nowSpec + 1) Gen.upperBoundNanos)

      let expectedDuration = Duration (timeSpecToDiffTime (endSpec - nowSpec))

      remainingDuration duration now (TimerRunning endTime) === expectedDuration

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
