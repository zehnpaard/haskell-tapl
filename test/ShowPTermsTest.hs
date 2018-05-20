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
    "ShowPTmVarTest 1:" ~: (show $ PTmVar "test") ~?= "test"
  , "ShowPTmVarTest 2:" ~: (show $ PTmVar "a") ~?= "a"
  , "ShowPTmVarTest 3:" ~: (show $ PTmVar "") ~?= ""
  ]

showPTmAbsTest :: Test
showPTmAbsTest = TestList [
    "ShowPTmAbsTest 1:" ~: 
       (show $ PTmAbs "x" TpBool $ PTmVar "x") ~?= 
       "λx:Bool.x"
  , "ShowPTmAbsTest 2:" ~: 
       (show $ PTmAbs "x" TpBool $ PTmVar "y") ~?= 
       "λx:Bool.y"
  , "ShowPTmAbsTest 3:" ~: 
       (show $ PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmVar "z") ~?= 
       "λx:Bool.λy:Bool.z"
  ]

showPTmAppTest :: Test
showPTmAppTest = TestList [
    "ShowPTmAppTest 1:" ~: 
       (show $ PTmApp (PTmVar "x") (PTmVar "y")) ~?= 
       "(x y)"
  , "ShowPTmAppTest 2:" ~: 
       (show $ PTmApp (PTmAbs "x" TpBool (PTmVar "x")) (PTmVar "y")) ~?= 
       "(λx:Bool.x y)"
  ]

showPTmTrueFalseTest :: Test
showPTmTrueFalseTest = TestList [
    "ShowPTmTrueFalseTest 1:" ~: (show PTmTrue) ~?= "true"
  , "ShowPTmTrueFalseTest 2:" ~: (show PTmFalse) ~?= "false"
  , "ShowPTmTrueFalseTest 3:" ~: 
       (show $ PTmAbs "x" TpBool PTmTrue) ~?= 
       "λx:Bool.true"
  , "ShowPTmTrueFalseTest 4:" ~: 
       (show $ PTmApp PTmFalse PTmTrue) ~?= 
       "(false true)"
  ]

showPTmIfTest :: Test
showPTmIfTest = TestList [
    "ShowPTmIfTest 1:" ~: 
       (show $ PTmIf PTmTrue PTmFalse PTmTrue) ~?= 
       "(if true then false else true)"
  , "ShowPTmIfTest 2:" ~: 
       (show $ PTmIf (PTmVar "x") PTmFalse (PTmAbs "x" TpBool PTmFalse)) ~?= 
       "(if x then false else λx:Bool.false)"
  ]
