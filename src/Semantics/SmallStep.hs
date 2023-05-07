{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Semantics.SmallStep where

import Data.Map as Map
import Language
import Semantics.Store

-- Small-step Semantics
-- The purpose of this module is to define
-- small step semantics for our language Expr a.



-- helper functions:
toExpr :: Bool -> Expr Bool
toExpr True = T
toExpr False = F


-- small-step semantics for Expr a 
step :: (Expr a, Store) -> Maybe (Expr a, Store)
step (Constant _, _) = Nothing
step (Var x, s) = do
    n <- Map.lookup x s
    pure (Constant n, s)
step (Op2 o e1 e2, s) = case (e1, e2) of
    (Constant m, Constant n) -> pure (Constant (m + n), s)
    (Constant _, _) -> do
        (e2', _) <- step (e2, s)
        pure (Op2 o e1 e2', s)
    _ -> do
        (e1', _) <- step (e1, s)
        pure (Op2 o e1' e2, s)

step (T, _) = Nothing
step (F, _) = Nothing
step (LessOrEq e1 e2, s) = case (e1, e2) of
    (Constant m, Constant n) -> pure (toExpr (m <= n), s)
    (Constant m, _) -> do
        (e2', _) <- step (e2, s)
        pure (LessOrEq (Constant m) e2', s)
    _ -> do
        (e1', _) <- step (e1, s)
        pure (LessOrEq e1' e2, s)

step (Skip, _) = Nothing
step (Assign x e, s) = case e of
    Constant n -> pure (Skip, Map.insert x n s)
    _ -> do
        (e', s') <- step (e, s)
        pure (Assign x e', s')
step (AndThen e1 e2, s) = case e1 of
    Skip -> pure (e2, s)
    _ -> do
        (e1', s') <- step (e1, s)
        pure (AndThen e1' e2, s')
step (IfElse b e1 e2, s) = case b of
    T -> pure (e1, s)
    F -> pure (e2, s)
    _ -> do
        (b', s') <- step (b, s)
        pure (IfElse b' e1 e2, s')
step (While b e, s) =
    pure (IfElse b (AndThen e (While b e)) Skip, s)



trace :: (Expr a , Store) -> [(Expr a, Store)]
trace (e, s) = case step (e, s) of
    Nothing -> [(e, s)]
    Just (e', s') -> (e, s) : trace (e', s')

-- TODO list :
-- 2. write big-step semantics and compare the two approaches
