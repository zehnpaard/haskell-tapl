import Test.HUnit

import Types
import Parser

main :: IO ()
main = do
  runTestTT $ TestList [showTmVarTest, parseTmVarTest]
  return ()

showTmVarTest :: Test
showTmVarTest = TestList [
  "Test 1:" ~: (show $ TmVar "test") ~?= "test",
  "Test 2:" ~: (show $ TmVar "a") ~?= "a",
  "Test 3:" ~: (show $ TmVar "") ~?= ""]

parseTmVarTest :: Test
parseTmVarTest = TestList [
  "Test 1:" ~: (readExpr "x") ~?= (TmVar "x"),
  "Test 2:" ~: (readExpr "abc") ~?= (TmVar "abc"),
  "Test 3:" ~: (readExpr "z1") ~?= (TmVar "z1")
  ]
