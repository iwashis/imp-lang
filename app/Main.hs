module Main(main) where

import Language as L
import Semantics.SmallStep
import Semantics.Store



main :: IO ()
main = do
    let e = L.testExpr
    print e
    print $ trace (e , emptyStore)
