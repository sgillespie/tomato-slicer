module System.Statusbar.Pomodoro.Run
  ( runTimer,
  ) where

import System.Statusbar.Pomodoro.Timer
  ( CurrentTime (..),
    Duration (..),
    Timer (..),
    newTimer,
    remainingDuration,
    resetTimer,
    tickTimer,
    timerStateText,
    toggleRunningTimer,
  )
import System.Statusbar.Pomodoro.Waybar
  ( WaybarOutput (..),
    formatTimerState,
    timerTooltipText,
  )

import Control.Concurrent (threadDelay)
import Data.Aeson qualified as Aeson
import Data.Default (Default (..))
import Data.Fixed (E12, Fixed (..), HasResolution (..), Pico)
import Data.Time (diffTimeToPicoseconds, secondsToDiffTime)
import System.Clock (Clock (..), getTime)
import System.Posix (Handler (..), installHandler, sigUSR1, sigUSR2)

runTimer :: Word -> IO ()
runTimer durationInSeconds = do
  let barOut = def
      duration = Duration . secondsToDiffTime $ fromIntegral durationInSeconds

  timerRef <- newIORef newTimer
  setupSignalHandlers duration timerRef

  void . infinitely $ do
    now <- CurrentTime <$> getTime Monotonic
    timer <- updateTimer timerRef (tickTimer now)
    printTimerState barOut duration now timer
    threadDelay 1_000_000

setupSignalHandlers :: Duration -> IORef Timer -> IO ()
setupSignalHandlers duration timerRef = do
  void $ installHandler sigUSR1 (Catch togglePaused) Nothing
  void $ installHandler sigUSR2 (Catch reset) Nothing
  where
    togglePaused = do
      now <- getTime Monotonic
      void $ updateTimer timerRef (toggleRunningTimer duration (CurrentTime now))

    reset = void $ updateTimer timerRef resetTimer

updateTimer :: IORef Timer -> (Timer -> Timer) -> IO Timer
updateTimer timerRef advance = do
  timer <- readIORef timerRef
  writeIORef timerRef (advance timer)
  readIORef timerRef

printTimerState :: WaybarOutput -> Duration -> CurrentTime -> Timer -> IO ()
printTimerState barOut duration now timer = putLBSLn . Aeson.encode $ barOut'
  where
    barOut' =
      barOut
        { wcoText = formatTimerState duration now timer,
          wcoAlt = Just (timerStateText timer),
          wcoClass = Just (timerStateText timer),
          wcoTooltip = Just (timerTooltipText duration now timer),
          wcoPercentage = Just $ percentage duration now timer
        }

percentage :: Duration -> CurrentTime -> Timer -> Word
percentage duration now timer =
  floor @Rational $ 100 * (1 - remaining / duration')
  where
    remaining = realToFrac $ getDuration (remainingDuration duration now timer)
    duration' = realToFrac (getDuration duration)
