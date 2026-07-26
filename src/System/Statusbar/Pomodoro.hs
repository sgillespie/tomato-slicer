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

  timerRef <- startTimer' durationInSeconds
  void $ setupSignalHandlers timerRef

  void . infinitely $ do
    now <- CurrentTime <$> getTime Monotonic -- Look up system time
    timer' <- updateTimer now timerRef tickTimer -- Advance timer
    printTimerState barOut now timer' -- Print json to stdout
    threadDelay 1_000_000 -- Update ~once/sec

startTimer' :: Word -> IO (IORef Timer)
startTimer' durationInSeconds = do
  t <- getTime Monotonic
  let durationInSeconds' = fromIntegral durationInSeconds
      timer = startTimer (CurrentTime t) (Duration $ secondsToDiffTime durationInSeconds')

  newIORef timer

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

printTimerState :: WaybarCustomOutput -> CurrentTime -> Timer -> IO ()
printTimerState barOut now timer = putLBSLn (Aeson.encode barOut')
  where
    barOut' = barOut {wcoText = formatTimerState now timer}

formatTimerState :: CurrentTime -> Timer -> Text
formatTimerState _ TimerDone = "Time is up!"
formatTimerState now timer = formatDuration $ remainingDuration now timer
