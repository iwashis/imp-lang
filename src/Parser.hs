{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Parser where

import Language
import Text.Parsec
    ( char,
      digit,
      letter,
      lower,
      spaces,
      string,
      choice,
      many1,
      parse,
      try,
      ParseError,
      Parsec )
import Text.Parsec.String hiding (Parser)
import Control.Applicative ((<|>), many)


type Parser = Parsec String ()
data SomeExpr where
    SomeArithmetic :: Expr Arithmetic -> SomeExpr
    SomeBool       :: Expr Bool       -> SomeExpr
    SomeComm       :: Expr Comm       -> SomeExpr
    deriving Show

someExprParser :: Parser SomeExpr
someExprParser = (SomeArithmetic <$> try parseArithmeticExpr)
         <|> (SomeBool <$> try parseBoolExpr)
         <|> (SomeComm <$> try parseCommExpr)


parseArithmeticExpr :: Parser (Expr Arithmetic)
parseArithmeticExpr = do 
    spaces
    expr <- try parseConstant <|> try parseVar <|> try parseOp2
    spaces
    pure expr

parseBoolExpr :: Parser (Expr Bool)
parseBoolExpr = do
    spaces
    expr <- try parseT <|> try parseF <|> try parseLessOrEq
    spaces
    return expr

parseCommExpr :: Parser (Expr Comm)
parseCommExpr = do
    spaces
    expr <- try parseSkip 
        <|> try parseAssign 
        <|> try parseAndThen 
        <|> try parseIfElse 
        <|> try parseWhile
    spaces
    return expr

parseVar :: Parser (Expr Arithmetic)
parseVar = Var <$> many1 lower

parseConstant :: Parser (Expr Arithmetic)
parseConstant = Constant . read <$> many1 digit

parseOp2 :: Parser (Expr Arithmetic)
parseOp2 = do
    char '('
    e1 <- parseArithmeticExpr
    op <- parseBinOp
    e2 <- parseArithmeticExpr
    char ')'
    return $ Op2 op e1 e2

parseBinOp :: Parser BinOp
parseBinOp = do
    spaces
    op <- choice [char '+', char '-', char '*', char '/']
    spaces
    case op of
        '+' -> return Add
        '-' -> return Sub
        '*' -> return Mul
        '/' -> return Div

parseT :: Parser (Expr Bool)
parseT = string "T" *> pure T

parseF :: Parser (Expr Bool)
parseF = string "F" *> pure F

parseLessOrEq :: Parser (Expr Bool)
parseLessOrEq = do
    char '('
    e1 <- parseArithmeticExpr
    spaces *> string "<=" <* spaces
    e2 <- parseArithmeticExpr
    char ')'
    return $ LessOrEq e1 e2

parseSkip :: Parser (Expr Comm)
parseSkip = string "Skip" *> pure Skip

parseAssign :: Parser (Expr Comm)
parseAssign = do
    v <- many1 letter
    spaces *> string ":=" <* spaces
    e <- parseArithmeticExpr
    return $ Assign v e

parseAndThen :: Parser (Expr Comm)
parseAndThen = do
    char '('
    e1 <- parseCommExpr
    spaces *> char ';' <* spaces
    e2 <- parseCommExpr
    char ')'
    return $ AndThen e1 e2

parseIfElse :: Parser (Expr Comm)
parseIfElse = do
    char '('
    spaces
    string "If"
    b <- parseBoolExpr
    spaces *> string "Then" <* spaces
    e1 <- parseCommExpr
    spaces *> string "Else" <* spaces
    e2 <- parseCommExpr
    spaces
    char ')'
    return $ IfElse b e1 e2

parseWhile :: Parser (Expr Comm)
parseWhile = do
    char '('
    spaces
    string "While"
    spaces
    b <- parseBoolExpr
    spaces *> string "Do" <* spaces
    --char '('
    e <- parseCommExpr
    char ')'
    return $ While b e

parseExpr :: Parser a -> String -> Either ParseError a
parseExpr p str = parse p "" str 

testExpr :: Either ParseError SomeExpr
testExpr = parseExpr someExprParser "While (x <= 2) Do x := (x + 1)"
