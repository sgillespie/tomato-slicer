module Main (main) where

import Spec qualified

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  specs' <- specs
  pure $ testGroup "Tests" [specs']

specs :: IO TestTree
specs = testGroup "(checked by Hspec)" <$> specs'
  where
    specs' :: IO [TestTree]
    specs' = do
      prelude <- testSpecs Spec.spec_prelude
      pure prelude
