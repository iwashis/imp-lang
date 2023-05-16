module Main where

import Gen.Language
import Language
import SemanticsProp
import Test.QuickCheck

main :: IO ()
main = quickCheck transitivity
