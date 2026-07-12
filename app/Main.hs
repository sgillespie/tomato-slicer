module Main (main) where

import Options.Applicative

{-# ANN Options ("HLint: ignore Use newtype instead of data" :: String) #-}
data Options = Options {optVerbose :: !Bool}
  deriving stock (Show)

main :: IO ()
main = execParser options >>= run

run :: Options -> IO ()
run opts = putTextLn ("Executable for pomodoro-waybar-module-hs: " <> show opts)

options :: ParserInfo Options
options =
  info (parser <**> helper) $
    fullDesc
      <> progDesc "Walking skeleton for a Pomodoro Waybar module"
      <> header "pomodoro-waybar-module-hs - Pomodoro Waybar module skeleton"

parser :: Parser Options
parser = Options <$> verboseOpt
  where
    verboseOpt =
      switch $
        long "verbose"
          <> short 'v'
          <> help "Verbose output?"
