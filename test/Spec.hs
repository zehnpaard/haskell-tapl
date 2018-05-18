import Test.HUnit

import ShowPTermsTest
import ParsePTermsTest

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
    evalTest
    ]
  return ()

showTermsTest :: Test
showTermsTest = TestList [
  "Test 1:" ~: (show $ TmVar 1) ~?= "1",
  "Test 2:" ~: (show $ TmAbs $ TmVar 1) ~?= "λ.1",
  "Test 3:" ~: (show $ TmApp (TmAbs $ TmVar 0) (TmAbs $ TmAbs $ TmApp (TmVar 0) (TmVar 1)))  ~?= 
     "(λ.0 λ.λ.(0 1))",
  "Test 4:" ~: (show TmTrue) ~?= "true",
  "Test 5:" ~: (show TmFalse) ~?= "false"
  ]

convertTermsTest :: Test
convertTermsTest = TestList [
  "Test 1:" ~: (convertTerm [] (PTmVar "x")) ~?= (TmVar (-1)),
  "Test 2:" ~: (convertTerm [] (PTmAbs "x" $ PTmVar "x")) ~?= (TmAbs $ TmVar 0),
  "Test 3:" ~: (convertTerm [] (PTmAbs "x" $ PTmVar "y")) ~?= (TmAbs $ TmVar (-1)),
  "Test 4:" ~: (convertTerm [] (PTmAbs "x" $ PTmAbs "y" $ PTmVar "y")) ~?= (TmAbs $ TmAbs $ TmVar 0),
  "Test 5:" ~: (convertTerm [] (PTmAbs "x" $ PTmAbs "y" $ PTmVar "x")) ~?= (TmAbs $ TmAbs $ TmVar 1),
  "Test 6:" ~: (convertTerm [] (PTmAbs "y" $ PTmAbs "x" $ PTmVar "y")) ~?= (TmAbs $ TmAbs $ TmVar 1),
  "Test 7:" ~: (convertTerm [] (PTmApp (PTmVar "x") (PTmVar "y"))) ~?= (TmApp (TmVar (-1)) (TmVar (-1))),
  "Test 8:" ~: (convertTerm [] (PTmApp (PTmAbs "x" $ PTmVar "x") (PTmAbs "x" $ PTmAbs "y" $ PTmVar "x"))) ~?=
    (TmApp (TmAbs $ TmVar 0) (TmAbs $ TmAbs $ TmVar 1)),
  "Test 9:" ~: (convertTerm [] (PTmApp (PTmAbs "x" $ PTmVar "x") (PTmAbs "y" $ PTmVar "x"))) ~?=
    (TmApp (TmAbs $ TmVar 0) (TmAbs $ TmVar (-1))),
  "Test 10:" ~: (convertTerm [] PTmTrue) ~?= TmTrue,
  "Test 11:" ~: (convertTerm [] PTmFalse) ~?= TmFalse
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
      (TmAbs $ TmAbs $ TmFalse)]
  ]

