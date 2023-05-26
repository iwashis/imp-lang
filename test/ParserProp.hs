module ParserProp where

import Language
import Parser
import Gen.Language

roundTrip :: SomeExpr -> Bool
roundTrip x = Right x == y
  where
    y = parseExpr someExprParser (show x)

constRoundTrip :: Int -> Bool
constRoundTrip x = Right (Constant $ abs x) == y
    where
        y = parseExpr parseConstant (show $ abs x)

varRoundTrip :: LowercaseString -> Bool
varRoundTrip (LowercaseString x) = Right (Var x) == y
    where 
        y = parseExpr parseVar x