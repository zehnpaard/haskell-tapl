module ShowPTermsTest (showPTermsTest) where

import Test.HUnit

import Types

showPTermsTest :: Test
showPTermsTest = TestList [
    showPTmVarTest, 
    showPTmAbsTest,
    showPTmAppTest,
    showPTmTrueFalseTest,
    showPTmIfTest
  ]

showPTmVarTest :: Test
showPTmVarTest = TestList [
  "Test 1:" ~: (show $ PTmVar "test") ~?= "test",
  "Test 2:" ~: (show $ PTmVar "a") ~?= "a",
  "Test 3:" ~: (show $ PTmVar "") ~?= ""
  ]

showPTmAbsTest :: Test
showPTmAbsTest = TestList [
  "Test 1:" ~: (show $ PTmAbs "x" TpBool $ PTmVar "x") ~?= "λx.x",
  "Test 2:" ~: (show $ PTmAbs "x" TpBool $ PTmVar "y") ~?= "λx.y",
  "Test 3:" ~: (show $ PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmVar "z") ~?= "λx.λy.z"
  ]

showPTmAppTest :: Test
showPTmAppTest = TestList [
  "Test 1:" ~: (show $ PTmApp (PTmVar "x") (PTmVar "y")) ~?= "(x y)",
  "Test 2:" ~: (show $ PTmApp (PTmAbs "x" TpBool (PTmVar "x")) (PTmVar "y")) ~?= "(λx.x y)"
  ]

showPTmTrueFalseTest :: Test
showPTmTrueFalseTest = TestList [
  "Test 1:" ~: (show PTmTrue) ~?= "true",
  "Test 2:" ~: (show PTmFalse) ~?= "false",
  "Test 3:" ~: (show $ PTmAbs "x" TpBool PTmTrue) ~?= "λx.true",
  "Test 4:" ~: (show $ PTmApp PTmFalse PTmTrue) ~?= "(false true)"
  ]

showPTmIfTest :: Test
showPTmIfTest = TestList [
  "Test 1:" ~: (show $ PTmIf PTmTrue PTmFalse PTmTrue) ~?= "(if true then false else true)",
  "Test 2:" ~: (show $ PTmIf (PTmVar "x") PTmFalse (PTmAbs "x" TpBool PTmFalse)) ~?= 
    "(if x then false else λx.false)"
  ]
