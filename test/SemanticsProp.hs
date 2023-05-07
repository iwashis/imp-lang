{-# LANGUAGE GADTs #-}

module SemanticsProp where


import Semantics.BigStep as BS
import Semantics.SmallStep as SS     

import Language
import Semantics.Store 

bigEqSmall :: (Eq a) => Expr a -> Store -> Bool
bigEqSmall e s = 
    let n = 100 
        maybeV  = BS.bigStep (e, s) n
        maybeV' = case SS.step (e, s) of
            Nothing -> Just (e,s) 
            Just (e',s') -> Just (e',s')
    in maybeV == do 
        (e',s') <- maybeV'
        BS.bigStep (e', s') (n-1)

transitivity :: SomeExpr -> Store -> Bool
transitivity (SomeInt e)  = bigEqSmall e
transitivity (SomeBool e) = bigEqSmall e
transitivity (SomeComm e) = bigEqSmall e