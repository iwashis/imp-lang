module Main (main) where

import Gen.Language ()
import Language
import Parser
import ParserProp
import SemanticsProp
import Test.Hspec
import Test.QuickCheck
import Text.Parsec
import Semantics.SmallStep
import Semantics.Store
import Semantics.BigStep
import Data.Map as Map

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
    describe "step test" $ do
            it "Op2 + 10 5, emptyStore" $ do
                step (Op2 Add (Constant 10) (Constant 5), emptyStore) 
                    `shouldBe` Just (Constant 15, emptyStore)
            it "LessOrEq 17 16, emptyStore" $ do
                step (LessOrEq (Constant 17) (Constant 16), emptyStore) 
                    `shouldBe` Just (F, emptyStore)
    describe "bigStep test" $ do
            it "LessOrEq 10 13, emptyStore" $ do
                bigStep (LessOrEq (Constant 10) (Constant 13), emptyStore) 10 
                    `shouldBe` Just True
            it "Op2 + 10 5, emptyStore 5" $ do
                bigStep (Op2 Add (Constant 10) (Constant 5), emptyStore) 5 
                    `shouldBe` Just 15  
            it  "Assign x 10 emptyStore 7" $ do
                bigStep (Assign "x" (Constant 10), emptyStore) 7
                    `shouldBe` Just (Map.insert "x" 10 emptyStore) 

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
