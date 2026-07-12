module Main (main) where

import Options.Applicative

{-# ANN Options ("HLint: ignore Use newtype instead of data" :: String) #-}
data Options = Options {optVerbose :: !Bool}
  deriving stock (Show)

main :: IO ()
main = execParser options >>= run

run :: Options -> IO ()
run opts = putTextLn ("Executable for tomato-slicer: " <> show opts)

options :: ParserInfo Options
options =
  info (parser <**> helper) $
    fullDesc
      <> progDesc "A tomato timer for JSON-speaking status bars"
      <> header "tomato-slicer - tomato timer status-bar module"

parser :: Parser Options
parser = Options <$> verboseOpt
  where
    verboseOpt =
      switch $
        long "verbose"
          <> short 'v'
          <> help "Verbose output?"
