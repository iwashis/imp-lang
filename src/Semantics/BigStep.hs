{-# LANGUAGE GADTs #-}

module Semantics.BigStep where
import Data.Map as Map
import Semantics.Store
import Language

toFunction :: BinOp -> (Int -> Int -> Int)
toFunction Add = (+)
toFunction Mul = (*)

stepAr :: (Expr Int, Store) -> Maybe Int
stepAr (Constant i, _) = Just i
stepAr (Var x, s) = Map.lookup x s
stepAr (Op2 o e1 e2, s) = do
  m <- stepAr (e1 , s)
  n <- stepAr (e2, s)
  pure $ toFunction o m n