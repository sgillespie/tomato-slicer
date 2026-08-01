{- | Main interface for timers, with only pure functions. Example usage:

      -- Get the system time from 'clock'
      now <- getTime Monotonic
      -- Start the timer
      timerRef <- newIORef $ startTimer (CurrentTime now) (secondsToDiffTime 10)
      forever $ do
        -- Get updated system time
        now' <- getTime Monotonic
        -- Update the timer
        timer <- readIORef timerRef
        timer' <- tickTimer timer (CurrentTime now')
        writeIORef timerRef timer'

         -- Read the timer state and format it
        let result =
              case timer' of
                TimerDone -> "Done"
                TimerRunning end -> formatTime (remainingTime (CurrentTime now') end) <> " remaining"

          putStrLn result
-}
module System.Statusbar.Pomodoro.Timer
  ( -- * Core timer types
    Timer (..),
    CurrentTime (..),
    EndTime (..),
    Duration (..),
    RemainingTime (..),

    -- * Operations on timers
    newTimer,
    startTimer,
    tickTimer,
    pauseTimer,
    resumeTimer,
    toggleRunningTimer,
    resetTimer,

    -- * Timer utilities
    timerStateText,
    remainingDuration,
    formatDuration,
    diffTimeToTimeSpec,
    timeSpecToDiffTime,
  ) where

import Data.Time (DiffTime, FormatTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Time qualified as Time
import System.Clock (TimeSpec, fromNanoSecs, toNanoSecs)

-- | The recorded current time, represented by 'TimeSpec'
newtype CurrentTime = CurrentTime {getCurrentTime :: TimeSpec}
  deriving stock (Eq, Ord, Show)

-- | The end time of a timer, represented by 'TimeSpec'
newtype EndTime = EndTime {getEndTime :: TimeSpec}
  deriving stock (Eq, Ord, Show)

newtype RemainingTime = RemainingTime {getRemainingTime :: TimeSpec}
  deriving stock (Eq, Ord, Show)

newtype Duration = Duration {getDuration :: DiffTime}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FormatTime, Num)

-- | Timer state: indicates whether it is running
data Timer
  = -- | Timer has not yet been started
    TimerReady
  | -- | Timer has expired
    TimerDone
  | -- | Timer is running
    TimerRunning
      EndTime
      -- ^ The timer deadline
  | -- | The time remaining on the timer
    TimerPaused
      RemainingTime
  deriving stock (Eq, Ord, Show)

newTimer :: Timer
newTimer = TimerReady

-- | Calculate the deadline and return a running 'Timer'
startTimer :: Duration -> CurrentTime -> Timer -> Timer
startTimer duration (CurrentTime now) TimerReady =
  TimerRunning . EndTime . (now +) . diffTimeToTimeSpec . getDuration $ duration
startTimer _ _ TimerDone = TimerDone
startTimer _ _ timer@(TimerPaused _) = timer
startTimer _ _ timer@(TimerRunning _) = timer

-- | Advance the timer. If expired, mark it as done.
tickTimer :: CurrentTime -> Timer -> Timer
tickTimer _ TimerReady = TimerReady
tickTimer _ TimerDone = TimerDone
tickTimer _ timer@(TimerPaused {}) = timer
tickTimer (CurrentTime now) timer@(TimerRunning (EndTime end))
  | now >= end = TimerDone
  | otherwise = timer

-- | Pause a running timer
pauseTimer :: CurrentTime -> Timer -> Timer
pauseTimer _ TimerReady = TimerReady
pauseTimer _ TimerDone = TimerDone
pauseTimer _ timer@(TimerPaused {}) = timer
pauseTimer (CurrentTime now) (TimerRunning end) =
  TimerPaused $ RemainingTime (getEndTime end - now)

-- | Resume a paused timer
resumeTimer :: CurrentTime -> Timer -> Timer
resumeTimer _ TimerReady = TimerReady
resumeTimer _ TimerDone = TimerDone
resumeTimer _ timer@(TimerRunning _) = timer
resumeTimer (CurrentTime now) (TimerPaused remaining) =
  TimerRunning $ EndTime (now + getRemainingTime remaining)

-- | Invert the pause state of a timer. In other words, if it is running, pause it; if it
-- is paused, resume it
toggleRunningTimer :: Duration -> CurrentTime -> Timer -> Timer
toggleRunningTimer duration now TimerReady = startTimer duration now TimerReady
toggleRunningTimer _ _ TimerDone = TimerDone
toggleRunningTimer _ now timer@(TimerRunning _) = pauseTimer now timer
toggleRunningTimer _ now timer@(TimerPaused _) = resumeTimer now timer

-- | Reset a timer to its ready state
resetTimer :: Timer -> Timer
resetTimer _ = TimerReady

timerStateText :: Timer -> Text
timerStateText TimerReady = "ready"
timerStateText TimerDone = "done"
timerStateText (TimerRunning _) = "running"
timerStateText (TimerPaused _) = "paused"

-- | Calculate the time left on a 'Timer'
remainingDuration :: Duration -> CurrentTime -> Timer -> Duration
remainingDuration duration _ TimerReady = duration
remainingDuration _ _ TimerDone = 0
remainingDuration _ _ (TimerPaused (RemainingTime remaining)) =
  Duration (timeSpecToDiffTime remaining)
remainingDuration _ (CurrentTime now) (TimerRunning (EndTime end)) =
  Duration $ timeSpecToDiffTime (end - now)

-- | Format the time in the form "MM:SS"
formatDuration :: Duration -> Text
formatDuration = toText . Time.formatTime Time.defaultTimeLocale "%0M:%0S"

-- | Transform a 'DiffTime' to 'TimeSpec'
diffTimeToTimeSpec :: DiffTime -> TimeSpec
diffTimeToTimeSpec diffTime = fromNanoSecs (diffTimeToPicoseconds diffTime `div` 1000)

-- | Transform a 'TimeSpec' to 'DiffTime'
timeSpecToDiffTime :: TimeSpec -> DiffTime
timeSpecToDiffTime spec = picosecondsToDiffTime (toNanoSecs spec * 1000)
