module Main where

import Language as L
import Parser as P
import Semantics.SmallStep

main :: IO ()
main = do
    let e = SomeComm L.testExpr
    print e
    print $ trace (e,emptyStore)
    print P.testExpr
