{-# LANGUAGE GADTs #-}

module Semantics.SmallStep where

import Data.Map as Map
import Language
import Semantics.Store


-- Semantics
-- The purpose of this module is to define
-- small step semantics for our language Expr a.



-- in order to handle Store values we simply use standard Map datatype
-- interface.

-- small step semantics for Int expressions present
-- in our language:
stepAr :: (Expr Int, Store) -> Maybe (Expr Int, Store)
stepAr (Constant _, _) = Nothing
stepAr (Var x, s) = do
    n <- Map.lookup x s
    pure (Constant n, s)
stepAr (Op2 o e1 e2, s) = case (e1, e2) of
    (Constant m, Constant n) -> pure (Constant (m + n), s)
    (Constant _, _) -> do
        (e2', _) <- stepAr (e2, s)
        pure (Op2 o e1 e2', s)
    _ -> do
        (e1', _) <- stepAr (e1, s)
        pure (Op2 o e1' e2, s)

toExpr :: Bool -> Expr Bool
toExpr True = T
toExpr False = F

stepBool :: (Expr Bool, Store) -> Maybe (Expr Bool, Store)
stepBool (T, _) = Nothing
stepBool (F, _) = Nothing
stepBool (LessOrEq e1 e2, s) = case (e1, e2) of
    (Constant m, Constant n) -> pure (toExpr (m <= n), s)
    (Constant m, _) -> do
        (e2', _) <- stepAr (e2, s)
        pure (LessOrEq (Constant m) e2', s)
    _ -> do
        (e1', _) <- stepAr (e1, s)
        pure (LessOrEq e1' e2, s)

stepCommand :: (Expr Store, Store) -> Maybe (Expr Store, Store)
stepCommand (Skip, _) = Nothing
stepCommand (Assign x e, s) = case e of
    Constant n -> pure (Skip, Map.insert x n s)
    _ -> do
        (e', s') <- stepAr (e, s)
        pure (Assign x e', s')
stepCommand (AndThen e1 e2, s) = case e1 of
    Skip -> pure (e2, s)
    _ -> do
        (e1', s') <- stepCommand (e1, s)
        pure (AndThen e1' e2, s')
stepCommand (IfElse b e1 e2, s) = case b of
    T -> pure (e1, s)
    F -> pure (e2, s)
    _ -> do
        (b', s') <- stepBool (b, s)
        pure (IfElse b' e1 e2, s')
stepCommand (While b e, s) =
    pure (IfElse b (AndThen e (While b e)) Skip, s)

trace :: (SomeExpr, Store) -> [(SomeExpr, Store)]
trace (SomeInt e, s) = case stepAr (e, s) of
    Nothing -> [(SomeInt e, s)]
    Just (e', s') -> (SomeInt e, s) : trace (SomeInt e', s')
trace (SomeBool e, s) = case stepBool (e, s) of
    Nothing -> [(SomeBool e, s)]
    Just (e', s') -> (SomeBool e, s) : trace (SomeBool e', s')
trace (SomeComm e, s) = case stepCommand (e, s) of
    Nothing -> [(SomeComm e, s)]
    Just (e', s') -> (SomeComm e, s) : trace (SomeComm e', s')

-- TODO list :
-- 2. write big-step semantics and compare the two approaches
