module System.Statusbar.Pomodoro.Waybar
  ( WaybarOutput (..),
    def,
    formatTimerState,
    timerTooltipText,
  ) where

import System.Statusbar.Pomodoro.Timer
  ( CurrentTime (..),
    Duration (..),
    Timer (..),
    formatDuration,
    remainingDuration, RemainingTime (..), EndTime (..), timeSpecToDiffTime,
  )

import Data.Aeson (KeyValue (..), ToJSON)
import Data.Aeson qualified as Aeson
import Data.Default (Default (..))
import System.Clock (TimeSpec)

data WaybarOutput = WaybarOutput
  { wcoText :: Text,
    wcoAlt :: Maybe Text,
    wcoTooltip :: Maybe Text,
    wcoClass :: Maybe Text,
    wcoPercentage :: Maybe Word
  }
  deriving stock (Generic, Show)

instance Default WaybarOutput where
  def =
    WaybarOutput
      { wcoText = "",
        wcoAlt = Nothing,
        wcoTooltip = Nothing,
        wcoClass = Nothing,
        wcoPercentage = Nothing
      }

instance ToJSON WaybarOutput where
  toJSON (WaybarOutput {..}) =
    Aeson.object
      [ "text" .= wcoText,
        "alt" .= wcoAlt,
        "tooltip" .= wcoTooltip,
        "class" .= wcoClass,
        "percentage" .= wcoPercentage
      ]

formatTimerState :: Duration -> CurrentTime -> Timer -> Text
formatTimerState duration now timer
  | remainingDuration duration now timer == 0 = "Time is up!"
  | otherwise = formatDuration (remainingDuration duration now timer)

timerTooltipText :: Duration -> CurrentTime -> Timer -> Text
timerTooltipText duration _ TimerReady = 
  "Ready (" <> formatDuration duration <> ")"
timerTooltipText _ _ TimerDone = "Complete"
timerTooltipText duration now timer@(TimerRunning _) = 
  "Running: " <> formatDuration (remainingDuration duration now timer) <> " remaining"
timerTooltipText duration now timer@(TimerPaused _) = 
  "Paused: " <> formatDuration (remainingDuration duration now timer) <> " remaining"

