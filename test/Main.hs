module Main (main) where

import System.Statusbar.Timer.TimerTest qualified as TimerTest

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "tomato-slicer" TimerTest.spec
