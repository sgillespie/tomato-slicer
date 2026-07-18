-- | Main interface for timers, with only pure functions. Example usage:
--
--       -- Get the system time from 'clock'
--       now <- getTime Monotonic
--       -- Start the timer
--       timerRef <- newIORef $ startTimer (CurrentTime now) (secondsToDiffTime 10)
--       forever $ do
--         -- Get updated system time
--         now' <- getTime Monotonic
--         -- Update the timer
--         timer <- readIORef timerRef
--         timer' <- tickTimer timer (CurrentTime now')
--         writeIORef timerRef timer'
--
--          -- Read the timer state and format it
--         let result =
--               case timer' of
--                 TimerDone -> "Done"
--                 TimerRunning end -> formatTime (remainingTime (CurrentTime now') end) <> " remaining"
--
--           putStrLn result
module System.Statusbar.Timer.Timer
  ( -- * Core timer types
    Timer (..),
    CurrentTime (..),
    EndTime (..),
    -- * Operations on timers
    startTimer,
    tickTimer,
    -- * Timer utilities
    remainingTime,
    formatTime,
  ) where

import Data.Text qualified as Text
import Data.Time (DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Time qualified as Time
import System.Clock (TimeSpec, fromNanoSecs, toNanoSecs)

-- | The recorded current time, represented by 'TimeSpec'
newtype CurrentTime = CurrentTime { getCurrentTime :: TimeSpec }
  deriving stock (Eq, Ord, Show)

-- | The end time of a timer, represented by 'TimeSpec'
newtype EndTime = EndTime { getEndTime :: TimeSpec }
  deriving stock (Eq, Ord, Show)

-- | Timer state: indicates whether it is running
data Timer
  = -- | Not running
    TimerDone
  | -- | Timer is running
    TimerRunning
      EndTime -- ^ The timer deadline

-- | Calculate the deadline and return a running 'Timer'
startTimer :: CurrentTime -> DiffTime -> Timer
startTimer (CurrentTime now) deadline =
  TimerRunning . EndTime $ now + fromNanoSecs (nanoSecs deadline)
  where
    nanoSecs diffTime = diffTimeToPicoseconds diffTime `div` 1000

-- | Calculate the time left on a 'Timer'
remainingTime :: CurrentTime -> Timer -> DiffTime
remainingTime _ TimerDone = 0
remainingTime (CurrentTime now) (TimerRunning (EndTime end)) =
  picosecondsToDiffTime $ picoSecs (end - now)
  where
    picoSecs time = toNanoSecs time * 1000

-- | Format the time in the form "MM:SS"
formatTime :: DiffTime -> Text
formatTime = Text.pack . Time.formatTime Time.defaultTimeLocale "%0M:%0S"

-- | Advance the timer. If expired, mark it as done.
tickTimer :: CurrentTime -> Timer -> Timer
tickTimer _ TimerDone = TimerDone
tickTimer (CurrentTime now) timer@(TimerRunning (EndTime end))
  | now >= end = TimerDone
  | otherwise = timer
