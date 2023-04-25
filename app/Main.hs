module Main where

import Language as L
import Parser as P

main :: IO ()
main = do
    print P.testExpr
