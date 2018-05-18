module ParsePTermsTest (parsePTermsTest) where

import Test.HUnit

import Types
import Parser

parsePTermsTest :: Test
parsePTermsTest = TestList [
    parsePTmVarTest,
    parsePTmAbsTest,
    parsePTmAppTest,
    parsePTmTrueFalseTest
  ]

parsePTmVarTest :: Test
parsePTmVarTest = TestList [
  "Test 1:" ~: (readExpr "x") ~?= (PTmVar "x"),
  "Test 2:" ~: (readExpr "abc") ~?= (PTmVar "abc"),
  "Test 3:" ~: (readExpr "z1") ~?= (PTmVar "z1")
  ]

parsePTmAbsTest :: Test
parsePTmAbsTest = TestList [
  "Test 1:" ~: (readExpr "λx.x") ~?= (PTmAbs "x" $ PTmVar "x"),
  "Test 2:" ~: (readExpr "λx.λy.x") ~?= (PTmAbs "x" $ PTmAbs "y" $ PTmVar "x")
  ]

parsePTmAppTest :: Test
parsePTmAppTest = TestList [
  "Test 1:" ~: (readExpr "(x y)") ~?= 
     (PTmApp (PTmVar "x") (PTmVar "y")),
  "Test 2:" ~: (readExpr "(λx.x y)") ~?= 
     (PTmApp (PTmAbs "x" $ PTmVar "x") (PTmVar "y")),
  "Test 3:" ~: (readExpr "(λx.x λz.y)") ~?= 
     (PTmApp (PTmAbs "x" $ PTmVar "x") (PTmAbs "z" $ PTmVar "y"))
  ]

parsePTmTrueFalseTest :: Test
parsePTmTrueFalseTest = TestList [
  "Test 1:" ~: (readExpr "true") ~?= PTmTrue,
  "Test 2:" ~: (readExpr "false") ~?= PTmFalse,
  "Test 3:" ~: (readExpr "λx.true") ~?= (PTmAbs "x" PTmTrue),
  "Test 4:" ~: (readExpr "(false true)") ~?= (PTmApp PTmFalse PTmTrue),
  "Test 5:" ~: (readExpr "λt.true") ~?= (PTmAbs "t" PTmTrue),
  "Test 6:" ~: (readExpr "λfalsex.false") ~?= (PTmAbs "falsex" PTmFalse)
  ]
