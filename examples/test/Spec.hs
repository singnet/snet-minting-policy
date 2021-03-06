{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

-- import qualified Spec.Game
import qualified Spec.StakingV1Trace
import           Test.Tasty
import           Test.Tasty.Hedgehog            ( HedgehogTestLimit(..) )

main :: IO ()
main = defaultMain tests

-- | Number of successful tests for each hedgehog property.
--   The default is 100 but we use a smaller number here in order to speed up
--   the test suite.
--
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 5)

tests :: TestTree
tests = localOption limit $ testGroup "Smart Contracts Test"
                                      [
    -- Spec.Game.tests
                                       Spec.StakingV1Trace.tests]
