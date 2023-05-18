module ParserProp (roundTrip) where

import Language
import Parser

roundTrip :: SomeExpr -> Bool
roundTrip x = Right x == y
  where
    y = parseExpr someExprParser (show x)
