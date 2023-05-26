{-# LANGUAGE GADTs #-}

module Parser where

import Control.Applicative ((<|>))
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
    _ <- char '('
    e1 <- parseIntExpr
    op <- parseBinOp
    e2 <- parseIntExpr
    _ <- char ')'
    pure $ Op2 op e1 e2

parseBinOp :: Parser BinOp
parseBinOp = do
    op <- choice [char '+', char '*']
    case op of
        '+' -> pure Add
        '*' -> pure Mul
        _ -> fail "unsupported operation type"

parseT :: Parser (Expr Bool)
parseT = string "T" >> pure T

parseF :: Parser (Expr Bool)
parseF = string "F" >> pure F

parseLessOrEq :: Parser (Expr Bool)
parseLessOrEq = do
    _ <- char '('
    e1 <- parseIntExpr
    _ <- string "<="
    e2 <- parseIntExpr
    _ <- char ')'
    pure $ LessOrEq e1 e2

parseSkip :: Parser (Expr Store)
parseSkip = string "Skip" >> pure Skip

parseAssign :: Parser (Expr Store)
parseAssign = do
    _ <- char '('
    v <- many1 letter
    spaces
    _ <- string ":="
    i <- parseIntExpr
    _ <- char ')'
    pure $ Assign v i

parseAndThen :: Parser (Expr Store)
parseAndThen = do
    _ <- char '('
    e1 <- parseCommExpr
    _ <- char ';'
    e2 <- parseCommExpr
    _ <- char ')'
    pure $ AndThen e1 e2

parseIfElse :: Parser (Expr Store)
parseIfElse = do
    _ <- char '('
    spaces
    _ <- string "If"
    b <- parseBoolExpr
    _ <- string "Then"
    e1 <- parseCommExpr
    _ <- string "Else"
    e2 <- parseCommExpr
    _ <- char ')'
    pure $ IfElse b e1 e2

parseWhile :: Parser (Expr Store)
parseWhile = do
    _ <- char '('
    spaces
    _ <- string "While"
    b <- parseBoolExpr
    _ <- string "Do"
    c <- parseCommExpr
    _ <- char ')'
    pure $ While b c

parseExpr :: Parser a -> String -> Either ParseError a
parseExpr p = parse p ""
