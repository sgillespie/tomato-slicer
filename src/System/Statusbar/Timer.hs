module System.Statusbar.Timer
  ( runTimer,
  ) where

import System.Statusbar.Timer.Timer
  ( CurrentTime (..),
    Timer (..),
    formatDuration,
    remainingDuration,
    startTimer,
    tickTimer, Duration (..),
  )

import Control.Concurrent (threadDelay)
import Data.Aeson (KeyValue (..), ToJSON)
import Data.Aeson qualified as Aeson
import Data.Time (secondsToDiffTime)
import System.Clock (Clock (..), getTime)

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

runTimer :: IO ()
runTimer = do
  -- Initialize waybar module output
  let barOut =
        WaybarCustomOutput
          { wcoText = "",
            wcoAlt = Nothing,
            wcoTooltip = Nothing,
            wcoClass = Nothing,
            wcoPercentage = Nothing
          }

  -- Start timer
  t <- getTime Monotonic
  timer <- newIORef $ startTimer (CurrentTime t) (Duration $ secondsToDiffTime 30)

  void . infinitely $ do
    -- Update the timer
    now <- getTime Monotonic
    timer' <- updateTimer (CurrentTime now) timer

    -- Format and send it to stdout
    let barOut' = barOut {wcoText = formatTimerState (CurrentTime now) timer'}
    putLBSLn (Aeson.encode barOut')

    -- Update ~once/sec
    threadDelay 1_000_000

updateTimer :: CurrentTime -> IORef Timer -> IO Timer
updateTimer now timerRef = do
  timer <- readIORef timerRef

  let timer' = tickTimer now timer
  writeIORef timerRef timer'

  pure timer'

formatTimerState :: CurrentTime -> Timer -> Text
formatTimerState _ TimerDone = "Time is up!"
formatTimerState now timer = formatDuration $ remainingDuration now timer
