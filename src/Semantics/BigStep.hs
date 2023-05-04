{-# LANGUAGE GADTs #-}

module Semantics.BigStep where
import Data.Map as Map
import Semantics.Store
import Language

toFunction :: BinOp -> (Int -> Int -> Int)
toFunction Add = (+)
toFunction Mul = (*)

bigStep :: (Expr a, Store) -> Maybe a
bigStep (Constant i, _) = Just i
bigStep (Var x, s) = Map.lookup x s
bigStep (Op2 o e1 e2, s) = do
  m <- bigStep (e1 , s)
  n <- bigStep (e2, s)
  pure $ toFunction o m n

bigStep (T , _) = pure True
bigStep (F, _ ) = pure False
bigStep (LessOrEq e1 e2, s) = do
  m <- bigStep (e1,s)
  n <- bigStep (e2,s)
  pure (m <= n)

bigStep (Skip, s) = pure s
bigStep (Assign x e, s) = do
  n <- bigStep (e, s)
  pure $ Map.insert x n s
bigStep (AndThen e1 e2, s) = do
  s1 <- bigStep (e1, s)
  bigStep (e2, s1)
bigStep (IfElse b e1 e2, s) = do
  bval <- bigStep ( b, s )
  if bval then bigStep (e1, s) else bigStep (e2, s)
bigStep (While b e, s) = do
  bval <- bigStep (b, s)
  if bval then do
    s' <- bigStep (e,s)
    bigStep (While b e, s') else pure s

