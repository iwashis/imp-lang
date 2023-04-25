{-# LANGUAGE GADTs #-}
module Semantics where

-- the purpose of this module is to define
-- small step semantics for our language Expr a.  

-- First we define Store which can be thought of as 
-- memory of stored values under variables that are
-- used in the languages.  
import Language
import Data.Map as Map

type VariableName = String
type Value = Int
type Store = Map VariableName Value

-- in order to handle Store values we simply use standard Map datatype
-- interface.  

-- small step semantics for Arithmetic expressions present
-- in our language:
stepAr :: (Expr Arithmetic, Store) -> Maybe (Expr Arithmetic, Store)
stepAr (Constant n, s) = Nothing
stepAr (Var x, s) = do
    n <- Map.lookup x s
    pure (Constant n ,s)
stepAr (Op2 o e1 e2, s) = case e1 of
    (Constant _) -> do
        (e2',_) <- step (e2, s)
        pure  (Op2 o e1 e2' ,s)
    _            -> do
        (e1',_) <- step (e1, s)
        pure  (Op2 o e1' e2, s)