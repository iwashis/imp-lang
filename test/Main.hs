module Main where

import Gen.Language
import SemanticsProp
import Test.QuickCheck
import Language
import ParserTest
import Test.HUnit

main :: IO ()
-- main = quickCheck transitivity
main = do
    runTestTTAndExit tests
