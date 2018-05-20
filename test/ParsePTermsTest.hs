module ParsePTermsTest (parsePTermsTest) where

import Test.HUnit

import Types
import Parser

parsePTermsTest :: Test
parsePTermsTest = TestList [
    parsePTmVarTest
  , parsePTmAbsTest
  , parsePTmAppTest
  , parsePTmTrueFalseTest
  , parsePTmIfTest
  , parseTypeTest
  ]

parsePTmVarTest :: Test
parsePTmVarTest = TestList [
    "ParsePTmVarTest 1:" ~: (readExpr "x") ~?= (PTmVar "x")
  , "ParsePTmVarTest 2:" ~: (readExpr "abc") ~?= (PTmVar "abc")
  , "ParsePTmVarTest 3:" ~: (readExpr "z1") ~?= (PTmVar "z1")
  ]

parsePTmAbsTest :: Test
parsePTmAbsTest = TestList [
    "ParsePTmAbsTest 1:" ~: 
       (readExpr "λx:Bool.x") ~?= 
       (PTmAbs "x" TpBool $ PTmVar "x")
  , "ParsePTmAbsTest 2:" ~: 
       (readExpr "λx:Bool.λy:Bool.x") ~?= 
       (PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmVar "x")
  ]

parsePTmAppTest :: Test
parsePTmAppTest = TestList [
    "ParsePTmAppTest 1:" ~: 
       (readExpr "(x y)") ~?= 
       (PTmApp (PTmVar "x") (PTmVar "y"))
  , "ParsePTmAppTest 2:" ~: 
       (readExpr "(λx:Bool.x y)") ~?= 
       (PTmApp (PTmAbs "x" TpBool $ PTmVar "x") (PTmVar "y"))
  , "ParsePTmAppTest 3:" ~: 
       (readExpr "(λx:Bool.x λz:Bool.y)") ~?= 
       (PTmApp (PTmAbs "x" TpBool $ PTmVar "x") (PTmAbs "z" TpBool $ PTmVar "y"))
  ]

parsePTmTrueFalseTest :: Test
parsePTmTrueFalseTest = TestList [
    "ParsePTmTrueFalseTest 1:" ~: (readExpr "true") ~?= PTmTrue
  , "ParsePTmTrueFalseTest 2:" ~: (readExpr "false") ~?= PTmFalse
  , "ParsePTmTrueFalseTest 3:" ~: 
       (readExpr "λx:Bool.true") ~?= 
       (PTmAbs "x" TpBool PTmTrue)
  , "ParsePTmTrueFalseTest 4:" ~: 
       (readExpr "(false true)") ~?= 
       (PTmApp PTmFalse PTmTrue)
  , "ParsePTmTrueFalseTest 5:" ~: 
       (readExpr "λt:Bool.true") ~?= 
       (PTmAbs "t" TpBool PTmTrue)
  , "ParsePTmTrueFalseTest 6:" ~: 
       (readExpr "λfalsex:Bool.false") ~?= 
       (PTmAbs "falsex" TpBool PTmFalse)
  ]

parsePTmIfTest :: Test
parsePTmIfTest = TestList [
    "ParsePTmIfTest 1:" ~: 
       (readExpr "(if true then false else true)") ~?= 
       (PTmIf PTmTrue PTmFalse PTmTrue)
  , "ParsePTmIfTest 2:" ~: 
       (readExpr "(if λx:Bool.x then false else false)") ~?= 
       (PTmIf (PTmAbs "x" TpBool $ PTmVar "x") PTmFalse PTmFalse)
  , "ParsePTmIfTest 3:" ~: 
       (readExpr "(if λx:Bool.x then λy:Bool.(y y) else z)") ~?= 
       (PTmIf (PTmAbs "x" TpBool $ PTmVar "x") 
              (PTmAbs "y" TpBool $ PTmApp (PTmVar "y") (PTmVar "y"))
              (PTmVar "z"))
  , "ParsePTmIfTest 4:" ~: 
       (readExpr "λip:Bool.(if ip then ifs else i)") ~?=
       (PTmAbs "ip" TpBool $ (PTmIf (PTmVar "ip") (PTmVar "ifs") (PTmVar "i")))
  ]

parseTypeTest :: Test
parseTypeTest = TestList [
    "ParseTypeTest 1:" ~:
       (readExpr "λx:Bool.x") ~?=
       (PTmAbs "x" TpBool $ PTmVar "x")
  , "ParseTypeTest 2:" ~:
       (readExpr "λx:Bool->Bool.x") ~?=
       (PTmAbs "x" (TpArrow TpBool TpBool) $ PTmVar "x")
  , "ParseTypeTest 3:" ~:
       (readExpr "λx:Bool->Bool->Bool.x") ~?=
       (PTmAbs "x" (TpArrow TpBool $ TpArrow TpBool TpBool) $ PTmVar "x")
  , "ParseTypeTest 4:" ~:
       (readExpr "λx:(Bool->Bool)->Bool.x") ~?=
       (PTmAbs "x" (TpArrow (TpArrow TpBool TpBool) TpBool ) $ PTmVar "x")
  ]
