module Main where

import Gen.Language
import SemanticsProp
import Test.QuickCheck
import Language

main :: IO ()
main = quickCheck transitivitySome
