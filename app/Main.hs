module Main (main) where

import System.Statusbar.Pomodoro (runServer, runStatus)

import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Options

data Options = Options
  { optCommand :: !Command,
    optVerbose :: !Bool
  }
  deriving stock (Show)

data Command
  = Serve ServeOptions
  | Status StatusOptions
  deriving stock (Eq, Ord, Show)

newtype ServeOptions = ServeOptions
  { optDuration :: Word
  }
  deriving stock (Eq, Ord, Show)

data StatusOptions = StatusOptions
  deriving stock (Eq, Ord, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  Options.execParser globalOptions >>= run

run :: Options -> IO ()
run Options {optCommand}
  | Serve (ServeOptions {optDuration}) <- optCommand = runServer optDuration
  | Status {} <- optCommand = runStatus

globalOptions :: ParserInfo Options
globalOptions =
  Options.info (parser <**> Options.helper) $
    Options.fullDesc
      <> Options.progDesc "A tomato timer for JSON-speaking status bars"
      <> Options.header "tomato-slicer - tomato timer status-bar module"

parser :: Parser Options
parser =
  Options
    <$> commandParser
    <*> verboseOpt

verboseOpt :: Parser Bool
verboseOpt =
  Options.switch $
    Options.long "verbose"
      <> Options.short 'v'
      <> Options.help "Verbose output?"

commandParser :: Parser Command
commandParser =
  Options.hsubparser $
    Options.command "serve" serveCommandOptions
      <> Options.command "status" statusCommandOptions

serveCommandOptions :: ParserInfo Command
serveCommandOptions = Options.info (Serve <$> serveCommandParser) serveCommandInfo
  where
    serveCommandInfo = Options.progDesc "Run the timer server in the foreground"

serveCommandParser :: Parser ServeOptions
serveCommandParser =
  ServeOptions
    <$> durationOpt

durationOpt :: Parser Word
durationOpt =
  Options.option Options.auto $
    Options.long "duration"
      <> Options.short 's'
      <> Options.value 30
      <> Options.showDefault
      <> Options.metavar "SECONDS"
      <> Options.help "Duration of the timer in seconds"

statusCommandOptions :: ParserInfo Command
statusCommandOptions = Options.info statusCommandParser statusCommandInfo
  where
    statusCommandParser = pure $ Status StatusOptions
    statusCommandInfo = Options.progDesc "Show the current timer status"
