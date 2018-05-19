import Test.HUnit

import ShowPTermsTest
import ParsePTermsTest
import ShowTmTypesTest

import Types
import Parser
import DeBruijn
import Evaluate

main :: IO ()
main = do
  runTestTT $ TestList [
    showPTermsTest,
    parsePTermsTest,
    showTermsTest,
    convertTermsTest,
    evalTest,
    showTmTypesTest
    ]
  return ()

showTermsTest :: Test
showTermsTest = TestList [
  "Test 1:" ~: (show $ TmVar 1) ~?= "1",
  "Test 2:" ~: (show $ TmAbs $ TmVar 1) ~?= "λ.1",
  "Test 3:" ~: (show $ TmApp (TmAbs $ TmVar 0) (TmAbs $ TmAbs $ TmApp (TmVar 0) (TmVar 1)))  ~?= 
     "(λ.0 λ.λ.(0 1))",
  "Test 4:" ~: (show TmTrue) ~?= "true",
  "Test 5:" ~: (show TmFalse) ~?= "false",
  "Test 6:" ~: (show $ TmIf TmTrue TmFalse TmFalse) ~?= "(if true then false else false)"
  ]

convertTermsTest :: Test
convertTermsTest = TestList [
  "Test 1:" ~: (convertTerm [] (PTmVar "x")) ~?= (TmVar (-1)),
  "Test 2:" ~: (convertTerm [] (PTmAbs "x" TpBool $ PTmVar "x")) ~?= (TmAbs $ TmVar 0),
  "Test 3:" ~: (convertTerm [] (PTmAbs "x" TpBool $ PTmVar "y")) ~?= (TmAbs $ TmVar (-1)),
  "Test 4:" ~: (convertTerm [] (PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmVar "y")) ~?= 
     (TmAbs $ TmAbs $ TmVar 0),
  "Test 5:" ~: (convertTerm [] (PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmVar "x")) ~?= (TmAbs $ TmAbs $ TmVar 1),
  "Test 6:" ~: (convertTerm [] (PTmAbs "y" TpBool $ PTmAbs "x" TpBool $ PTmVar "y")) ~?= (TmAbs $ TmAbs $ TmVar 1),
  "Test 7:" ~: (convertTerm [] (PTmApp (PTmVar "x") (PTmVar "y"))) ~?= (TmApp (TmVar (-1)) (TmVar (-1))),
  "Test 8:" ~: (convertTerm [] (PTmApp (PTmAbs "x" TpBool $ PTmVar "x") (PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmVar "x"))) ~?=
    (TmApp (TmAbs $ TmVar 0) (TmAbs $ TmAbs $ TmVar 1)),
  "Test 9:" ~: (convertTerm [] (PTmApp (PTmAbs "x" TpBool $ PTmVar "x") (PTmAbs "y" TpBool $ PTmVar "x"))) ~?=
    (TmApp (TmAbs $ TmVar 0) (TmAbs $ TmVar (-1))),
  "Test 10:" ~: (convertTerm [] PTmTrue) ~?= TmTrue,
  "Test 11:" ~: (convertTerm [] PTmFalse) ~?= TmFalse,
  "Test 12:" ~: (convertTerm [] (PTmIf PTmTrue 
                                       PTmFalse 
                                       (PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmApp (PTmVar "y") (PTmVar "x")))) ~?=
    (TmIf TmTrue TmFalse $ TmAbs $ TmAbs $ TmApp (TmVar 0) (TmVar 1))
  ]

evalTest :: Test
evalTest = TestList [
  "Test 1:" ~: (substitute (TmVar 1) (TmVar 0)) ~?= (TmVar 1),
  "Test 2:" ~: (substitute (TmVar 1) (TmAbs $ TmVar 0)) ~?= (TmAbs $ TmVar 0),
  "Test 3:" ~: (substitute (TmVar 3) (TmAbs $ TmVar 1)) ~?= (TmAbs $ TmVar 3),
  "Test 4:" ~: (eval (TmAbs $ TmVar 1)) ~?= [(TmAbs $ TmVar 1)],
  "Test 5:" ~: (eval (TmApp (TmAbs $ TmAbs $ TmVar 1) (TmAbs $ TmVar 0))) ~?= 
     [(TmApp (TmAbs $ TmAbs $ TmVar 1) (TmAbs $ TmVar 0)),
      (TmAbs $ TmAbs $ TmVar 0)],
  "Test 6:" ~: (eval (TmApp (TmAbs $ TmVar 0) (TmAbs $ TmAbs $ TmVar 1))) ~?= 
     [(TmApp (TmAbs $ TmVar 0) (TmAbs $ TmAbs $ TmVar 1)),
      (TmAbs $ TmAbs $ TmVar 1)],
  "Test 7:" ~: (eval TmTrue) ~?= [TmTrue],
  "Test 8:" ~: (eval TmFalse) ~?= [TmFalse],
  "Test 9:" ~: (eval (TmApp (TmAbs $ TmAbs $ TmVar 1) (TmAbs $ TmFalse))) ~?= 
     [(TmApp (TmAbs $ TmAbs $ TmVar 1) (TmAbs $ TmFalse)),
      (TmAbs $ TmAbs $ TmFalse)],
  "Test 10:" ~: (eval (TmIf TmTrue TmFalse TmTrue)) ~?= [(TmIf TmTrue TmFalse TmTrue), TmFalse],
  "Test 11:" ~: (eval (TmIf (TmApp (TmAbs $ TmVar 0) TmTrue) (TmAbs $ TmVar 0) (TmAbs $ TmFalse))) ~?=
     [(TmIf (TmApp (TmAbs $ TmVar 0) TmTrue) (TmAbs $ TmVar 0) (TmAbs $ TmFalse)),
      (TmIf TmTrue (TmAbs $ TmVar 0) (TmAbs $ TmFalse)),
      (TmAbs $ TmVar 0)]
  ]

