module System.Statusbar.Pomodoro.Run
  ( runTimer,
  ) where

import System.Statusbar.Pomodoro.Timer
  ( CurrentTime (..),
    Duration (..),
    Timer (..),
    resetTimer,
    startTimer,
    tickTimer,
    togglePausedTimer,
  )
import System.Statusbar.Pomodoro.Waybar
  ( WaybarOutput (..),
    formatTimerState,
  )

import Control.Concurrent (threadDelay)
import Data.Aeson qualified as Aeson
import Data.Time (secondsToDiffTime)
import System.Clock (Clock (..), getTime)
import System.Posix (Handler (..), installHandler, sigUSR1, sigUSR2)

runTimer :: Word -> IO ()
runTimer durationInSeconds = do
  let barOut =
        WaybarOutput
          { wcoText = "",
            wcoAlt = Nothing,
            wcoTooltip = Nothing,
            wcoClass = Nothing,
            wcoPercentage = Nothing
          }

  timerRef <- startTimer' durationInSeconds
  setupSignalHandlers timerRef

  void . infinitely $ do
    now <- CurrentTime <$> getTime Monotonic
    timer <- updateTimer timerRef (tickTimer now)
    printTimerState barOut now timer
    threadDelay 1_000_000

startTimer' :: Word -> IO (IORef Timer)
startTimer' durationInSeconds = do
  now <- CurrentTime <$> getTime Monotonic
  let duration = Duration . secondsToDiffTime $ fromIntegral durationInSeconds
  newIORef $ startTimer now duration

setupSignalHandlers :: IORef Timer -> IO ()
setupSignalHandlers timerRef = do
  void $ installHandler sigUSR1 (Catch togglePaused) Nothing
  void $ installHandler sigUSR2 (Catch reset) Nothing
  where
    togglePaused = do
      now <- getTime Monotonic
      void $ updateTimer timerRef (togglePausedTimer (CurrentTime now))

    reset = void $ updateTimer timerRef resetTimer

updateTimer :: IORef Timer -> (Timer -> Timer) -> IO Timer
updateTimer timerRef advance = do
  timer <- readIORef timerRef
  writeIORef timerRef (advance timer)
  readIORef timerRef

printTimerState :: WaybarOutput -> CurrentTime -> Timer -> IO ()
printTimerState barOut now timer = putLBSLn . Aeson.encode $ barOut'
  where
    barOut' = barOut {wcoText = formatTimerState now timer}
