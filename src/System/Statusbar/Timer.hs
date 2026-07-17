{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Statusbar.Timer
  ( runTimer,
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson (ToJSON, KeyValue (..))
import Control.Concurrent (threadDelay)

data WaybarCustomOutput = WaybarCustomOutput
  { wcoText :: Text,
    wcoAlt :: Maybe Text,
    wcoTooltip :: Maybe Text,
    wcoClass :: Maybe Text,
    wcoPercentage :: Maybe Word
  }
  deriving stock (Generic, Show)

instance ToJSON WaybarCustomOutput where
  toJSON (WaybarCustomOutput{..}) =
    Aeson.object
      [ "text" .= wcoText,
        "alt" .= wcoAlt,
        "tooltip" .= wcoTooltip,
        "class" .= wcoClass,
        "percentage" .= wcoPercentage
      ]

runTimer :: IO ()
runTimer = do
  let out = WaybarCustomOutput
        { wcoText = "Hello, Waybar!",
          wcoAlt = Nothing,
          wcoTooltip = Nothing,
          wcoClass = Nothing,
          wcoPercentage = Nothing
        }

  forever $ do
    putLBSLn (Aeson.encode out)
    threadDelay 1_000_000

