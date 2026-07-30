module System.Statusbar.Pomodoro
  ( -- * Timer types
    Timer (..),
    CurrentTime (..),
    EndTime (..),
    Duration (..),
    RemainingTime (..),

    -- * Timer operations
    startTimer,
    tickTimer,
    pauseTimer,
    resumeTimer,
    toggleRunningTimer,
    resetTimer,

    -- * Runner
    runTimer,

    -- * Waybar output
    WaybarOutput (..),
  ) where

import System.Statusbar.Pomodoro.Run (runTimer)
import System.Statusbar.Pomodoro.Timer
  ( CurrentTime (..),
    Duration (..),
    EndTime (..),
    RemainingTime (..),
    Timer (..),
    pauseTimer,
    resetTimer,
    resumeTimer,
    startTimer,
    tickTimer,
    toggleRunningTimer,
  )
import System.Statusbar.Pomodoro.Waybar (WaybarOutput (..))
