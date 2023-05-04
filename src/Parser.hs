{-# LANGUAGE GADTs #-}

module Parser where

-- TODO: fix imports. Perhaps use a formatter on the whole project

import Control.Applicative (many, (<|>))
import Language
import Semantics.Store
import Text.Parsec (
    ParseError,
    Parsec,
    char,
    choice,
    digit,
    letter,
    lower,
    many1,
    parse,
    spaces,
    string,
    try,
 )

type Parser = Parsec String ()

someExprParser :: Parser SomeExpr
someExprParser =
    (SomeInt <$> try parseIntExpr)
        <|> (SomeBool <$> try parseBoolExpr)
        <|> (SomeComm <$> try parseCommExpr)

parseIntExpr :: Parser (Expr Int)
parseIntExpr = do
    spaces
    expr <- try parseConstant <|> try parseVar <|> try parseOp2
    spaces
    pure expr

parseBoolExpr :: Parser (Expr Bool)
parseBoolExpr = do
    spaces
    expr <- try parseT <|> try parseF <|> try parseLessOrEq
    spaces
    pure expr

parseCommExpr :: Parser (Expr Store)
parseCommExpr = do
    spaces
    expr <-
        try parseSkip
            <|> try parseAssign
            <|> try parseAndThen
            <|> try parseIfElse
            <|> try parseWhile
    spaces
    pure expr

parseVar :: Parser (Expr Int)
parseVar = Var <$> many1 lower

parseConstant :: Parser (Expr Int)
parseConstant = Constant . read <$> many1 digit

parseOp2 :: Parser (Expr Int)
parseOp2 = do
    char '('
    e1 <- parseIntExpr
    op <- parseBinOp
    e2 <- parseIntExpr
    char ')'
    pure $ Op2 op e1 e2

parseBinOp :: Parser BinOp
parseBinOp = do
    spaces
    op <- choice [char '+', char '-', char '*', char '/']
    spaces
    case op of
        '+' -> pure Add
        '*' -> pure Mul
   

parseT :: Parser (Expr Bool)
parseT = string "T" *> pure T

parseF :: Parser (Expr Bool)
parseF = string "F" *> pure F

parseLessOrEq :: Parser (Expr Bool)
parseLessOrEq = do
    char '('
    e1 <- parseIntExpr
    spaces *> string "<=" <* spaces
    e2 <- parseIntExpr
    char ')'
    pure $ LessOrEq e1 e2

parseSkip :: Parser (Expr Store)
parseSkip = string "Skip" *> pure Skip

parseAssign :: Parser (Expr Store)
parseAssign = do
    v <- many1 letter
    spaces *> string ":=" <* spaces
    e <- parseIntExpr
    pure $ Assign v e

parseAndThen :: Parser (Expr Store)
parseAndThen = do
    char '('
    e1 <- parseCommExpr
    spaces *> char ';' <* spaces
    e2 <- parseCommExpr
    char ')'
    pure $ AndThen e1 e2

parseIfElse :: Parser (Expr Store)
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
    pure $ IfElse b e1 e2

parseWhile :: Parser (Expr Store)
parseWhile = do
    -- char '('
    spaces
    string "While"
    spaces
    b <- parseBoolExpr
    spaces *> string "Do" <* spaces
    -- char '('
    e <- parseCommExpr
    -- char ')'
    pure $ While b e

parseExpr :: Parser a -> String -> Either ParseError a
parseExpr p = parse p ""

-- TODO: move unit tests to future test suite
testInput :: String
testInput = "While (x <= 2) Do Skip"

-- TODO: something is wrong with the testInput and is not parsed correctly
testExpr :: Either ParseError SomeExpr
testExpr = parseExpr someExprParser testInput
