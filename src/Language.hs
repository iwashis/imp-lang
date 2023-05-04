{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Language (Expr(..), BinOp (..), Arithmetic (..), Comm(..), SomeExpr (..),  testExpr) where


data BinOp = Add | Sub | Mul | Div deriving (Eq)

instance Show BinOp where
    show :: BinOp -> String
    show Add = "+"
    show Mul = "*"
    show Div = "/"
    show Sub = "-"

data Arithmetic = Arithmetic
    deriving (Eq, Show)
data Comm = Comm
    deriving (Eq, Show)

data Expr a where
    Var      :: String -> Expr Arithmetic -- basic
    Constant :: Int    -> Expr Arithmetic -- basic
    Op2      :: BinOp  -> Expr Arithmetic -> Expr Arithmetic -> Expr Arithmetic --compo
    T        :: Expr Bool -- basic
    F        :: Expr Bool -- basic
    LessOrEq :: Expr Arithmetic -> Expr Arithmetic -> Expr Bool --compo
    Skip     :: Expr Comm -- basic
    Assign   :: String      -> Expr Arithmetic -> Expr Comm -- basic
    AndThen  :: Expr Comm   -> Expr Comm -> Expr Comm -- compo
    IfElse   :: Expr Bool   -> Expr Comm -> Expr Comm -> Expr Comm -- compo
    While    :: Expr Bool   -> Expr Comm -> Expr Comm -- compo

-- 120 
-- x
-- x <= 10
-- x <= y
-- inst1 ; instr2 
instance Show a => Show (Expr a) where
    show :: Show a => Expr a -> String
    show (Var v) = v
    show (Constant c) = show c
    show (Op2 op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show T = "T"
    show F = "F"
    show (LessOrEq e1 e2) = "(" ++ show e1 ++ " <= " ++ show e2 ++ ")"
    show Skip = "Skip"
    show (Assign v e) = v ++ " := " ++ show e
    show (AndThen e1 e2) = "(" ++ show e1 ++ "; " ++ show e2 ++ ")"
    show (IfElse b e1 e2) = "(" ++ "If " ++ show b 
                            ++ " Then " ++ show e1 ++ " Else " ++ show e2 ++ ")"
    show (While b e) = "(" ++ "While " ++ show b ++ " Do " ++ show e ++ ")"

instance Eq a => Eq (Expr a) where
    (==) :: Expr a -> Expr a -> Bool
    (Var s1) == (Var s2)                         = s1 == s2
    (Constant n1) == (Constant n2)               = n1 == n2
    (Op2 op1 e11 e12) == (Op2 op2 e21 e22)       = op1 == op2 && e11 == e21 && e12 == e22
    T == T                                       = True
    F == F                                       = True
    (LessOrEq e11 e12) == (LessOrEq e21 e22)     = e11 == e21 && e12 == e22
    Skip == Skip                                 = True
    (Assign s1 e1) == (Assign s2 e2)             = s1 == s2 && e1 == e2
    (AndThen c11 c12) == (AndThen c21 c22)       = c11 == c21 && c12 == c22
    (IfElse b1 c11 c12) == (IfElse b2 c21 c22)   = b1 == b2 && c11 == c21 && c12 == c22
    (While b1 c1) == (While b2 c2)               = b1 == b2 && c1 == c2
    _ == _                                       = False

testExpr :: Expr Comm
testExpr = AndThen 
    (Assign "x" (Constant 0)) 
    (While (LessOrEq (Var "x") (Constant 3)) $ AndThen Skip (Assign "x" (Op2 Add (Var "x") (Constant 2))))

data SomeExpr where
    SomeArithmetic :: Expr Arithmetic -> SomeExpr
    SomeBool       :: Expr Bool -> SomeExpr
    SomeComm       :: Expr Comm -> SomeExpr


instance Show SomeExpr where
    show :: SomeExpr -> String
    show (SomeArithmetic e) = show e
    show (SomeBool e) = show e 
    show (SomeComm e) = show e

