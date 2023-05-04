{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gen.Language where

import Language

import Test.QuickCheck


-- Define a generator for Arithmetic expressions
genArithmeticExpr :: Gen (Expr Arithmetic)
genArithmeticExpr = oneof 
    [ Var <$> arbitrary -- generate a variable expression with a random string name
    , Constant <$> arbitrary -- generate a constant expression with a random integer value
    , Op2 <$> genOp <*> genArithmeticExpr <*> genArithmeticExpr -- generate an arithmetic operation expression with two sub-expressions
    ]
    where genOp = elements [Add, Sub, Mul, Div] 

-- Define a generator for Boolean expressions
genBoolExpr :: Gen (Expr Bool)
genBoolExpr = oneof
    [ pure T -- generate a true constant expression
    , pure F -- generate a false constant expression
    , LessOrEq <$> genArithmeticExpr <*> genArithmeticExpr -- generate a less than or equal to comparison expression with two arithmetic sub-expressions
    ]

-- Define a generator for Comm expressions
genCommExpr :: Gen (Expr Comm)
genCommExpr = oneof 
    [ pure Skip -- generate a skip expression
    , Assign <$> arbitrary <*> genArithmeticExpr -- generate an assignment expression with a random string variable name and an arithmetic expression
    , AndThen <$> genCommExpr <*> genCommExpr -- generate a sequential composition expression with two sub-expressions
    , IfElse <$> genBoolExpr <*> genCommExpr <*> genCommExpr -- generate an if-else expression with a boolean expression and two sub-expressions
    , While <$> genBoolExpr <*> genCommExpr -- generate a while loop expression with a boolean expression and a sub-expression
    ]

