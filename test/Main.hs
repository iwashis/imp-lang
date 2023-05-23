module Main (main) where

import Gen.Language ()
import Language
import Parser
import ParserProp
import SemanticsProp
import Test.Hspec
import Test.QuickCheck
import Text.Parsec

main :: IO ()
main = hspec $ do
    describe "IMP parser" $ do
        it "small step semantic" $
            property transitivity
        describe "parse.show = id" $ do
            it "Expr" $
                property roundTrip
            it "Int" $
                property constRoundTrip 
            it "LowercaseString" $
                property varRoundTrip 
        describe "parser only + and *" $ do
            it "+" $ do
                parseExpr parseBinOp "+" `shouldBe` Right Add
            it "*" $ do
                parseExpr parseBinOp "*" `shouldBe` Right Mul
            it "-" $ do
                parseBinOp `shouldFailOn` "-"
            it "/" $ do
                parseBinOp `shouldFailOn` "/"
        describe "Constants parse correctly " $ do
            it "Skip" $ do
                parseExpr parseSkip "Skip" `shouldBe` Right Skip
            it "T" $ do 
                parseExpr parseT "T" `shouldBe` Right T
            it "F" $ do 
                parseExpr parseF "F" `shouldBe` Right F

shouldFailOn ::
    (HasCallStack, Show a) =>
    -- | Parser that takes stream and produces result or error message
    Parser a ->
    -- | Input that the parser should fail on
    String ->
    Expectation
p `shouldFailOn` s = case parse p "" s of
    Left _ -> return ()
    Right v ->
        expectationFailure $
            "the parser is expected to fail, but it parsed: " ++ show v
