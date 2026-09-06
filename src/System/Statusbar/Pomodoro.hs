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

    -- * Server interface
    ServerEnv (..),
    ServerT (..),
    ServerError (..),
    runServerT,

    -- * Client interface types
    ClientError (..),

    -- * Runners
    runServer,
    runStatus,

    -- * Waybar output
    WaybarOutput (..),
  ) where

import System.Statusbar.Pomodoro.Client (runStatus)
import System.Statusbar.Pomodoro.Error (ClientError (..), ServerError (..))
import System.Statusbar.Pomodoro.Server
  ( ServerEnv (..),
    ServerT (..),
    runServer,
    runServerT,
  )
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
