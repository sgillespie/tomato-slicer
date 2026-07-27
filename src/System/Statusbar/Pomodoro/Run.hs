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

      durationInSeconds' = fromIntegral durationInSeconds

  t <- getTime Monotonic
  timer <- newIORef $ startTimer (CurrentTime t) (Duration $ secondsToDiffTime durationInSeconds')
  setupSignalHandlers timer

  void . infinitely $ do
    now <- getTime Monotonic
    timer' <- updateTimer timer (tickTimer (CurrentTime now))

    let barOut' = barOut {wcoText = formatTimerState (CurrentTime now) timer'}
    putLBSLn (Aeson.encode barOut')

    threadDelay 1_000_000

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
