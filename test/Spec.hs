import Test.HUnit

import Types
import Parser

main :: IO ()
main = do
  runTestTT $ TestList [
    showTmVarTest, 
    parseTmVarTest,
    showTmAbsTest,
    parseTmAbsTest
    ]
  return ()

showTmVarTest :: Test
showTmVarTest = TestList [
  "Test 1:" ~: (show $ TmVar "test") ~?= "test",
  "Test 2:" ~: (show $ TmVar "a") ~?= "a",
  "Test 3:" ~: (show $ TmVar "") ~?= ""
  ]

parseTmVarTest :: Test
parseTmVarTest = TestList [
  "Test 1:" ~: (readExpr "x") ~?= (TmVar "x"),
  "Test 2:" ~: (readExpr "abc") ~?= (TmVar "abc"),
  "Test 3:" ~: (readExpr "z1") ~?= (TmVar "z1")
  ]

showTmAbsTest :: Test
showTmAbsTest = TestList [
  "Test 1:" ~: (show $ TmAbs "x" $ TmVar "x") ~?= "λx.x",
  "Test 2:" ~: (show $ TmAbs "x" $ TmVar "y") ~?= "λx.y",
  "Test 3:" ~: (show $ TmAbs "x" $ TmAbs "y" $ TmVar "z") ~?= "λx.λy.z"
  ]

parseTmAbsTest :: Test
parseTmAbsTest = TestList [
  "Test 1:" ~: (readExpr "λx.x") ~?= (TmAbs "x" $ TmVar "x"),
  "Test 1:" ~: (readExpr "λx.λy.x") ~?= (TmAbs "x" $ TmAbs "y" $ TmVar "x")
  ]
