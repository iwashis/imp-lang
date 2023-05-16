module Main (main) where

import Gen.Language ()
import SemanticsProp
import Test.QuickCheck

main :: IO ()
main = quickCheck transitivity
