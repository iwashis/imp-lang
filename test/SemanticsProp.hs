{-# LANGUAGE GADTs #-}

module SemanticsProp where


import Semantics.BigStep as BS
import Semantics.SmallStep as SS     

import Language
import Semantics.Store 

transitivity :: (Eq a) => Expr a -> Store -> Bool
transitivity e s = 
    let maybeV  = BS.bigStep (e, s)
        maybeV' = do 
            (e',s') <- SS.step (e, s) 
            BS.bigStep (e',s')
    in maybeV == maybeV'

transitivitySome :: SomeExpr -> Store -> Bool
transitivitySome (SomeInt e) = transitivity e
transitivitySome (SomeBool e) = transitivity e
transitivitySome (SomeComm e) = transitivity e