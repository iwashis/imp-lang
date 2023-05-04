{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Gen.Language where

import Language

import Test.QuickCheck
import Data.Typeable hiding (TypeRep)

instance Arbitrary BinOp where
  arbitrary = elements [Add, Sub, Mul, Div]

-- Define a generator for Arithmetic expressions
genArithmeticExpr :: Gen (Expr Arithmetic)
genArithmeticExpr = oneof 
    [ Var <$> arbitrary -- generate a variable expression with a random string name
    , Constant <$> arbitrary -- generate a constant expression with a random integer value
    , Op2 <$> arbitrary <*> genArithmeticExpr <*> genArithmeticExpr -- generate an arithmetic operation expression with two sub-expressions
    ]

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


-- Type representative
data MyTypeRep a where
  TypeArithmetic :: MyTypeRep Arithmetic
  TypeBool       :: MyTypeRep Bool
  TypeComm       :: MyTypeRep Comm

-- Function to obtain MyTypeRep for Typeable types
myTypeRep :: forall a. Typeable a => Proxy a -> MyTypeRep a
myTypeRep _ = case eqT :: Maybe (a :~: Arithmetic) of
    Just Refl -> TypeArithmetic
    Nothing   -> case eqT :: Maybe (a :~: Bool) of
        Just Refl -> TypeBool
        Nothing   -> case eqT :: Maybe (a :~: Comm) of
            Just Refl -> TypeComm
            Nothing   -> error "Invalid type"

-- Define a generator for Expr a
genExpr :: forall a. (Typeable a) => Gen (Expr a)
genExpr = genExprWithRep (myTypeRep (Proxy :: Proxy a))

-- Helper function to generate Expr a based on the type representative
genExprWithRep :: MyTypeRep a -> Gen (Expr a)
genExprWithRep TypeArithmetic = genArithmeticExpr
genExprWithRep TypeBool       = genBoolExpr
genExprWithRep TypeComm       = genCommExpr
