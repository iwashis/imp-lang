{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Language (SomeExpr (..)) where

import Language
import Semantics.Store
import Test.QuickCheck

-- Define a generator for Int expressions
genIntExpr :: Gen (Expr Int)
genIntExpr =
    oneof
        [ Var <$> arbitrary -- generate a variable expression with a random string name
        , Constant <$> arbitrary -- generate a constant expression with a random integer value
        , Op2 <$> genOp <*> genIntExpr <*> genIntExpr -- generate an arithmetic operation expression with two sub-expressions
        ]
  where
    genOp = elements [Add, Mul]

-- Define a generator for Boolean expressions
genBoolExpr :: Gen (Expr Bool)
genBoolExpr =
    oneof
        [ pure T -- generate a true constant expression
        , pure F -- generate a false constant expression
        , LessOrEq <$> genIntExpr <*> genIntExpr -- generate a less than or equal to comparison expression with two arithmetic sub-expressions
        ]

-- Define a generator for Comm expressions
genCommExpr :: Gen (Expr Store)
genCommExpr =
    oneof
        [ pure Skip -- generate a skip expression
        , Assign <$> arbitrary <*> genIntExpr -- generate an assignment expression with a random string variable name and an arithmetic expression
        , AndThen <$> genCommExpr <*> genCommExpr -- generate a sequential composition expression with two sub-expressions
        , IfElse <$> genBoolExpr <*> genCommExpr <*> genCommExpr -- generate an if-else expression with a boolean expression and two sub-expressions
        , While <$> genBoolExpr <*> genCommExpr -- generate a while loop expression with a boolean expression and a sub-expression
        ]

instance Arbitrary SomeExpr where
    arbitrary = do
        x <- oneof $ pure <$> ["int", "bool", "store"]
        case x of
            "int" -> some <$> genIntExpr
            "bool" -> some <$> genBoolExpr
            _ -> some <$> genCommExpr
