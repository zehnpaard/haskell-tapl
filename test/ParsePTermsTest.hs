module ParsePTermsTest (parsePTermsTest) where

import Test.HUnit

import Types
import Parser

parsePTermsTest :: Test
parsePTermsTest = TestList [
    parsePTmVarTest,
    parsePTmAbsTest,
    parsePTmAppTest,
    parsePTmTrueFalseTest,
    parsePTmIfTest
  ]

parsePTmVarTest :: Test
parsePTmVarTest = TestList [
  "Test 1:" ~: (readExpr "x") ~?= (PTmVar "x"),
  "Test 2:" ~: (readExpr "abc") ~?= (PTmVar "abc"),
  "Test 3:" ~: (readExpr "z1") ~?= (PTmVar "z1")
  ]

parsePTmAbsTest :: Test
parsePTmAbsTest = TestList [
  "Test 1:" ~: (readExpr "λx.x") ~?= (PTmAbs "x" TpBool $ PTmVar "x"),
  "Test 2:" ~: (readExpr "λx.λy.x") ~?= (PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmVar "x")
  ]

parsePTmAppTest :: Test
parsePTmAppTest = TestList [
  "Test 1:" ~: (readExpr "(x y)") ~?= 
     (PTmApp (PTmVar "x") (PTmVar "y")),
  "Test 2:" ~: (readExpr "(λx.x y)") ~?= 
     (PTmApp (PTmAbs "x" TpBool $ PTmVar "x") (PTmVar "y")),
  "Test 3:" ~: (readExpr "(λx.x λz.y)") ~?= 
     (PTmApp (PTmAbs "x" TpBool $ PTmVar "x") (PTmAbs "z" TpBool $ PTmVar "y"))
  ]

parsePTmTrueFalseTest :: Test
parsePTmTrueFalseTest = TestList [
  "Test 1:" ~: (readExpr "true") ~?= PTmTrue,
  "Test 2:" ~: (readExpr "false") ~?= PTmFalse,
  "Test 3:" ~: (readExpr "λx.true") ~?= (PTmAbs "x" TpBool PTmTrue),
  "Test 4:" ~: (readExpr "(false true)") ~?= (PTmApp PTmFalse PTmTrue),
  "Test 5:" ~: (readExpr "λt.true") ~?= (PTmAbs "t" TpBool PTmTrue),
  "Test 6:" ~: (readExpr "λfalsex.false") ~?= (PTmAbs "falsex" TpBool PTmFalse)
  ]

parsePTmIfTest :: Test
parsePTmIfTest = TestList [
  "Test 1:" ~: (readExpr "(if true then false else true)") ~?= (PTmIf PTmTrue PTmFalse PTmTrue),
  "Test 2:" ~: (readExpr "(if λx.x then false else false)") ~?= 
    (PTmIf (PTmAbs "x" TpBool $ PTmVar "x") PTmFalse PTmFalse),
  "Test 3:" ~: (readExpr "(if λx.x then λy.(y y) else z)") ~?= 
    (PTmIf (PTmAbs "x" TpBool $ PTmVar "x") 
           (PTmAbs "y" TpBool $ PTmApp (PTmVar "y") (PTmVar "y"))
           (PTmVar "z")),
  "Test 4:" ~: (readExpr "λip.(if ip then ifs else i)") ~?=
    (PTmAbs "ip" TpBool $ (PTmIf (PTmVar "ip") (PTmVar "ifs") (PTmVar "i")))
  ]
