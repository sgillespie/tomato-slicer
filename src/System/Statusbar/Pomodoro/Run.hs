module System.Statusbar.Pomodoro.Run
  ( runTimer,
  ) where

import System.Statusbar.Pomodoro.Timer
  ( CurrentTime (..),
    Duration (..),
    Timer (..),
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
import System.Posix (Handler (..), installHandler, sigUSR1)

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
  void $ setupSignalHandlers timer

  void . infinitely $ do
    now <- getTime Monotonic
    timer' <- updateTimer (CurrentTime now) timer tickTimer

    let barOut' = barOut {wcoText = formatTimerState (CurrentTime now) timer'}
    putLBSLn (Aeson.encode barOut')

    threadDelay 1_000_000

setupSignalHandlers :: IORef Timer -> IO Handler
setupSignalHandlers timerRef = do
  installHandler sigUSR1 (Catch togglePaused) Nothing
  where
    togglePaused = do
      now <- getTime Monotonic
      void $ updateTimer (CurrentTime now) timerRef togglePausedTimer

updateTimer :: CurrentTime -> IORef Timer -> (CurrentTime -> Timer -> Timer) -> IO Timer
updateTimer now timerRef advance = do
  timer <- readIORef timerRef

  let timer' = advance now timer
  writeIORef timerRef timer'

  pure timer'
