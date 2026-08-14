module System.Statusbar.Pomodoro.Run
  ( runDaemon,
    runTimer,
  ) where

import System.Statusbar.Pomodoro.Timer
  ( CurrentTime (..),
    Duration (..),
    Timer (..),
    newTimer,
    remainingDuration,
    resetTimer,
    tickTimer,
    timerStateText,
    toggleRunningTimer,
  )
import System.Statusbar.Pomodoro.Waybar
  ( WaybarOutput (..),
    formatTimerState,
    timerTooltipText,
  )

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally)
import Control.Monad.Extra (untilJustM)
import Data.Aeson qualified as Aeson
import Data.Bits ((.|.))
import Data.Default (Default (..))
import Data.Time (secondsToDiffTime)
import System.Clock (Clock (..), getTime)
import System.FilePath ((</>))
import System.Posix (Fd, Handler (..), OpenFileFlags (..), OpenMode (..), addSignal, changeWorkingDirectory, closeFd, createSession, defaultFileFlags, dupTo, emptySignalSet, forkProcess, getProcessID, installHandler, nullFileMode, openFd, ownerReadMode, ownerWriteMode, removeLink, setFileCreationMask, sigHUP, sigILL, sigINT, sigQUIT, sigTERM, sigTRAP, sigUSR1, sigUSR2, stdError, stdInput, stdOutput)
import System.Posix.ByteString (fdWrite)
import System.XDG (getRuntimeDir)
import Prelude hiding (readFile)

runDaemon :: IO ()
runDaemon = do
  xdgRunDir <- getRuntimeDir
  let pidFile = xdgRunDir </> "tomato-slicer.pid"

  runBackground pidFile $ do
    done <- newIORef Nothing
    let doneHandler = writeIORef done (Just ())
        quitSignals =
          [ sigHUP,
            sigINT,
            sigQUIT,
            sigILL,
            sigTRAP,
            sigTERM
          ]
    mapM_ (\s -> installHandler s (Catch doneHandler) Nothing) quitSignals

    untilJustM $ do
      done' <- readIORef done

      maybe
        (threadDelay 1_000_000)
        (const $ cleanupPidFile pidFile)
        done'

      pure done'

runBackground :: FilePath -> IO a -> IO ()
runBackground pidFile action = do
  -- Create pid file
  fd <- openFdNewExclusive pidFile WriteOnly

  -- Fork
  void . forkProcess $ do
    -- Setsid
    _ <- createSession
    -- Fork again
    void . forkProcess $ do
      -- Write the PID
      daemonPid <- getProcessID
      _ <- fdWrite fd (show daemonPid) `finally` closeFd fd
      -- Remap std handles to /dev/null
      mapM_ (connectFd "/dev/null") [stdInput, stdOutput, stdError]
      -- Reset umask to 0
      _ <- setFileCreationMask nullFileMode
      -- Change pwd to /
      changeWorkingDirectory "/"

      void action

connectFd :: FilePath -> Fd -> IO ()
connectFd srcPath destHandle =
  bracket
    (openFd srcPath ReadOnly defaultFileFlags)
    closeFd
    (\fd -> void $ dupTo fd destHandle)

openFdNewExclusive :: FilePath -> OpenMode -> IO Fd
openFdNewExclusive file rwMode = openFd file rwMode flags
  where
    flags =
      defaultFileFlags
        { exclusive = True,
          creat = Just (ownerReadMode .|. ownerWriteMode)
        }

cleanupPidFile :: FilePath -> IO ()
cleanupPidFile = removeLink

runTimer :: Word -> IO ()
runTimer durationInSeconds = do
  let barOut = def
      duration = Duration . secondsToDiffTime $ fromIntegral durationInSeconds

  timerRef <- newIORef newTimer
  setupSignalHandlers duration timerRef

  void . infinitely $ do
    now <- CurrentTime <$> getTime Monotonic
    timer <- updateTimer timerRef (tickTimer now)
    printTimerState barOut duration now timer
    threadDelay 1_000_000

setupSignalHandlers :: Duration -> IORef Timer -> IO ()
setupSignalHandlers duration timerRef = do
  void $ installHandler sigUSR1 (Catch togglePaused) Nothing
  void $ installHandler sigUSR2 (Catch reset) Nothing
  where
    togglePaused = do
      now <- getTime Monotonic
      void $ updateTimer timerRef (toggleRunningTimer duration (CurrentTime now))

    reset = void $ updateTimer timerRef resetTimer

updateTimer :: IORef Timer -> (Timer -> Timer) -> IO Timer
updateTimer timerRef advance = do
  timer <- readIORef timerRef
  writeIORef timerRef (advance timer)
  readIORef timerRef

printTimerState :: WaybarOutput -> Duration -> CurrentTime -> Timer -> IO ()
printTimerState barOut duration now timer = putLBSLn . Aeson.encode $ barOut'
  where
    barOut' =
      barOut
        { wcoText = formatTimerState duration now timer,
          wcoAlt = Just (timerStateText timer),
          wcoClass = Just (timerStateText timer),
          wcoTooltip = Just (timerTooltipText duration now timer),
          wcoPercentage = Just $ percentage duration now timer
        }

percentage :: Duration -> CurrentTime -> Timer -> Word
percentage duration now timer =
  floor @Rational $ 100 * (1 - remaining / duration')
  where
    remaining = realToFrac $ getDuration (remainingDuration duration now timer)
    duration' = realToFrac (getDuration duration)
