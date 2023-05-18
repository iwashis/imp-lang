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
        it "parse.show = id" $
            property roundTrip
        describe "parser only + and *" $ do
            it "+" $ do
                parseExpr parseBinOp "+" `shouldBe` Right Add
            it "*" $ do
                parseExpr parseBinOp "*" `shouldBe` Right Mul
            it "-" $ do
                parseBinOp `shouldFailOn` "-"
            it "/" $ do
                parseBinOp `shouldFailOn` "/"

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
