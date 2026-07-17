module Main (main) where

import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Options
import System.Statusbar.Timer (runTimer)

{-# ANN Options ("HLint: ignore Use newtype instead of data" :: String) #-}
data Options = Options {optVerbose :: !Bool}
  deriving stock (Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  Options.execParser options >>= run

run :: Options -> IO ()
run _ = runTimer

options :: ParserInfo Options
options =
  Options.info (parser <**> Options.helper) $
    Options.fullDesc
      <> Options.progDesc "A tomato timer for JSON-speaking status bars"
      <> Options.header "tomato-slicer - tomato timer status-bar module"

parser :: Parser Options
parser = Options <$> verboseOpt
  where
    verboseOpt =
      Options.switch $
        Options.long "verbose"
          <> Options.short 'v'
          <> Options.help "Verbose output?"
