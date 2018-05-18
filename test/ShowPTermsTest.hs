module ShowPTermsTest (showPTermsTest) where

import Test.HUnit

import Types

showPTermsTest :: Test
showPTermsTest = TestList [
    showPTmVarTest, 
    showPTmAbsTest,
    showPTmAppTest
  ]

showPTmVarTest :: Test
showPTmVarTest = TestList [
  "Test 1:" ~: (show $ PTmVar "test") ~?= "test",
  "Test 2:" ~: (show $ PTmVar "a") ~?= "a",
  "Test 3:" ~: (show $ PTmVar "") ~?= ""
  ]

showPTmAbsTest :: Test
showPTmAbsTest = TestList [
  "Test 1:" ~: (show $ PTmAbs "x" $ PTmVar "x") ~?= "λx.x",
  "Test 2:" ~: (show $ PTmAbs "x" $ PTmVar "y") ~?= "λx.y",
  "Test 3:" ~: (show $ PTmAbs "x" $ PTmAbs "y" $ PTmVar "z") ~?= "λx.λy.z"
  ]

showPTmAppTest :: Test
showPTmAppTest = TestList [
  "Test 1:" ~: (show $ PTmApp (PTmVar "x") (PTmVar "y")) ~?= "(x y)",
  "Test 2:" ~: (show $ PTmApp (PTmAbs "x" (PTmVar "x")) (PTmVar "y")) ~?= "(λx.x y)"
  ]



