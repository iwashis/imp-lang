{-# LANGUAGE GADTs #-}

module Semantics.BigStep (bigStep) where

import Data.Map as Map
import Language
import Semantics.Store

-- bigstep semantics with a boundary condition given by number n
bigStep :: (Expr a, Store) -> Int -> Maybe a
bigStep _ 0 = Nothing
bigStep (Constant i, _) _ = Just i
bigStep (Var x, s) _ = Map.lookup x s
bigStep (Op2 o e1 e2, s) n = do
    x <- bigStep (e1, s) (n - 1)
    y <- bigStep (e2, s) (n - 1)
    pure $ toFunction o x y
bigStep (T, _) _ = pure True
bigStep (F, _) _ = pure False
bigStep (LessOrEq e1 e2, s) n = do
    m1 <- bigStep (e1, s) (n - 1)
    m2 <- bigStep (e2, s) (n - 1)
    pure (m1 <= m2)
bigStep (Skip, s) _ = pure s
bigStep (Assign x e, s) n = do
    y <- bigStep (e, s) (n - 1)
    pure $ Map.insert x y s
bigStep (AndThen e1 e2, s) n = do
    s1 <- bigStep (e1, s) (n - 1)
    bigStep (e2, s1) (n - 1)
bigStep (IfElse b e1 e2, s) n = do
    bval <- bigStep (b, s) (n - 1)
    if bval then bigStep (e1, s) (n - 1) else bigStep (e2, s) (n - 1)
bigStep (While b e, s) n = do
    bval <- bigStep (b, s) (n - 1)
    if bval
        then do
            s' <- bigStep (e, s) (n - 1)
            bigStep (While b e, s') (n - 1)
        else pure s
