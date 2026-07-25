module Main (main) where

import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Options
import System.Statusbar.Pomodoro (runTimer)

data Options = Options
  { optDuration :: Word,
    optVerbose :: !Bool
  }
  deriving stock (Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  Options.execParser options >>= run

run :: Options -> IO ()
run Options {..} = runTimer optDuration

options :: ParserInfo Options
options =
  Options.info (parser <**> Options.helper) $
    Options.fullDesc
      <> Options.progDesc "A tomato timer for JSON-speaking status bars"
      <> Options.header "tomato-slicer - tomato timer status-bar module"

parser :: Parser Options
parser =
  Options
    <$> durationOpt
    <*> verboseOpt

durationOpt :: Parser Word
durationOpt =
  Options.option Options.auto $
    Options.long "duration"
      <> Options.short 'd'
      <> Options.value 30
      <> Options.showDefault
      <> Options.metavar "SECONDS"
      <> Options.help "Duration of the timer in seconds"

verboseOpt :: Parser Bool
verboseOpt =
  Options.switch $
    Options.long "verbose"
      <> Options.short 'v'
      <> Options.help "Verbose output?"
