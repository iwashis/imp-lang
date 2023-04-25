module Main where

import Language as L
import Parser as P
import Semantics as S

main :: IO ()
main = do
    print P.testExpr
