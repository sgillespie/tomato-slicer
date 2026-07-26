module System.Statusbar.Pomodoro.Waybar
  ( WaybarOutput (..),
    formatTimerState,
  ) where

import System.Statusbar.Pomodoro.Timer
  ( CurrentTime,
    Timer (..),
    formatDuration,
    remainingDuration,
  )

import Data.Aeson (KeyValue (..), ToJSON)
import Data.Aeson qualified as Aeson

data WaybarOutput = WaybarOutput
  { wcoText :: Text,
    wcoAlt :: Maybe Text,
    wcoTooltip :: Maybe Text,
    wcoClass :: Maybe Text,
    wcoPercentage :: Maybe Word
  }
  deriving stock (Generic, Show)

instance ToJSON WaybarOutput where
  toJSON (WaybarOutput {..}) =
    Aeson.object
      [ "text" .= wcoText,
        "alt" .= wcoAlt,
        "tooltip" .= wcoTooltip,
        "class" .= wcoClass,
        "percentage" .= wcoPercentage
      ]

formatTimerState :: CurrentTime -> Timer -> Text
formatTimerState _ TimerDone = "Time is up!"
formatTimerState now timer = formatDuration $ remainingDuration now timer
