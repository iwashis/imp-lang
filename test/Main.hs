module Main (main) where

import Gen.Language ()
import SemanticsProp
import Test.QuickCheck
import ParserProp
import Test.Hspec
import Parser
import Language
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

shouldFailOn
  :: (HasCallStack, Show a)
  => Parser a               -- ^ Parser that takes stream and produces result or error message
  -> String                 -- ^ Input that the parser should fail on
  -> Expectation
p `shouldFailOn` s = case parse p "" s of
  Left _ -> return ()
  Right v -> expectationFailure $
    "the parser is expected to fail, but it parsed: " ++ show v