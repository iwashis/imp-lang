module ParserTest where 
    
import Test.HUnit
import Parser

-- wystarcza testy someExprParser (pozostale funkcje sa w niej wykorzystywane, a sprawdzamy wszystkie mozliwe typy danych) 
-- ewentualnie analogicznie mozna dopisac testy pozostalych funkcji, ale w tym przypadku, latwo dojsc dzieki tym testom, gdzie wystepuje blad 

tests :: Test
tests = TestList [ 
    TestCase $ assertEqual "Test 1" (show <$> parseExpr someExprParser "(While (x <= 10) Do (If T Then Skip Else (x := 13)))") (Right "(While (x <= 10) Do (If T Then Skip Else (x := 13)))") 
    , TestCase $ assertEqual "Test 2" (show <$> parseExpr someExprParser "(If (x <= 20) Then ((While (x <= 2) Do Skip); (x := 13)) Else (x := 10))") (Right "(If (x <= 20) Then ((While (x <= 2) Do Skip); (x := 13)) Else (x := 10))") 
    , TestCase $ assertEqual "Test 3" (show <$> parseExpr someExprParser "((x := 1) ; ((y := 2) ; ((While (x <= 10) Do (If (y <= 4) Then (x := (x + y)) Else Skip)); (If (y <= 0) Then (y := (y + x)) Else (y := (y * x)))))))") (Right "((x := 1); ((y := 2); ((While (x <= 10) Do (If (y <= 4) Then (x := (x + y)) Else Skip)); (If (y <= 0) Then (y := (y + x)) Else (y := (y * x))))))") 
    , TestCase $ assertEqual "Test Var" (show <$> parseExpr someExprParser "x") (Right "x") 
    , TestCase $ assertEqual "Test Constant" (show <$> parseExpr someExprParser "12345") (Right "12345") 
    , TestCase $ assertEqual "Test Op 1" (show <$> parseExpr someExprParser "(x + y)") (Right "(x + y)") 
    , TestCase $ assertEqual "Test Op 2" (show <$> parseExpr someExprParser "(a * b)") (Right "(a * b)") 
    , TestCase $ assertEqual "Test T" (show <$> parseExpr someExprParser "T") (Right "T") 
    , TestCase $ assertEqual "Test F" (show <$> parseExpr someExprParser "F") (Right "F") 
    , TestCase $ assertEqual "Test LessOrEq" (show <$> parseExpr someExprParser "(6 <= y)") (Right "(6 <= y)") 
    , TestCase $ assertEqual "Test Skip" (show <$> parseExpr someExprParser "Skip") (Right "Skip")
    , TestCase $ assertEqual "Test Assign" (show <$> parseExpr someExprParser "(x := 7)") (Right "(x := 7)") 
    , TestCase $ assertEqual "Test AndThen" (show <$> parseExpr someExprParser "(Skip ; Skip)") (Right "(Skip; Skip)") 
    , TestCase $ assertEqual "Test IfElse" (show <$> parseExpr someExprParser "(If F Then Skip Else Skip)") (Right "(If F Then Skip Else Skip)") 
    , TestCase $ assertEqual "Test While" (show <$> parseExpr someExprParser "(While T Do Skip)") (Right "(While T Do Skip)") ]