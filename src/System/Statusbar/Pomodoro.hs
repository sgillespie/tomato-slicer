module System.Statusbar.Pomodoro
  ( runTimer,
  ) where

import System.Statusbar.Pomodoro.Timer
  ( CurrentTime (..),
    Duration (..),
    Timer (..),
    formatDuration,
    remainingDuration,
    startTimer,
    tickTimer,
    togglePausedTimer,
  )

import Control.Concurrent (threadDelay)
import Data.Aeson (KeyValue (..), ToJSON)
import Data.Aeson qualified as Aeson
import Data.Time (secondsToDiffTime)
import System.Clock (Clock (..), getTime)
import System.Posix (Handler (..), installHandler, sigUSR1)

data WaybarCustomOutput = WaybarCustomOutput
  { wcoText :: Text,
    wcoAlt :: Maybe Text,
    wcoTooltip :: Maybe Text,
    wcoClass :: Maybe Text,
    wcoPercentage :: Maybe Word
  }
  deriving stock (Generic, Show)

instance ToJSON WaybarCustomOutput where
  toJSON (WaybarCustomOutput {..}) =
    Aeson.object
      [ "text" .= wcoText,
        "alt" .= wcoAlt,
        "tooltip" .= wcoTooltip,
        "class" .= wcoClass,
        "percentage" .= wcoPercentage
      ]

runTimer :: Word -> IO ()
runTimer durationInSeconds = do
  -- Initialize waybar module output
  let barOut =
        WaybarCustomOutput
          { wcoText = "",
            wcoAlt = Nothing,
            wcoTooltip = Nothing,
            wcoClass = Nothing,
            wcoPercentage = Nothing
          }

      durationInSeconds' = fromIntegral durationInSeconds

  -- Start timer
  t <- getTime Monotonic
  timer <- newIORef $ startTimer (CurrentTime t) (Duration $ secondsToDiffTime durationInSeconds')
  -- Handle pause/continue
  void $ setupSignalHandlers timer

  void . infinitely $ do
    -- Update the timer
    now <- getTime Monotonic
    timer' <- updateTimer (CurrentTime now) timer tickTimer

    -- Format and send it to stdout
    let barOut' = barOut {wcoText = formatTimerState (CurrentTime now) timer'}
    putLBSLn (Aeson.encode barOut')

    -- Update ~once/sec
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

formatTimerState :: CurrentTime -> Timer -> Text
formatTimerState _ TimerDone = "Time is up!"
formatTimerState now timer = formatDuration $ remainingDuration now timer
