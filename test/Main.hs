module Main (main) where

import Gen.Language ()
import SemanticsProp
import Test.QuickCheck
import ParserProp

main :: IO ()
main = do
    quickCheck transitivity
    quickCheck roundTrip
