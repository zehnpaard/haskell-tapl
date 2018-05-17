import Test.HUnit

import Types
import Parser
import DeBruijn
import Evaluate

main :: IO ()
main = do
  runTestTT $ TestList [
    showPTmVarTest, 
    parsePTmVarTest,
    showPTmAbsTest,
    parsePTmAbsTest,
    showPTmAppTest,
    parsePTmAppTest,
    showTermsTest,
    convertTermsTest,
    evalTest
    ]
  return ()

showPTmVarTest :: Test
showPTmVarTest = TestList [
  "Test 1:" ~: (show $ PTmVar "test") ~?= "test",
  "Test 2:" ~: (show $ PTmVar "a") ~?= "a",
  "Test 3:" ~: (show $ PTmVar "") ~?= ""
  ]

parsePTmVarTest :: Test
parsePTmVarTest = TestList [
  "Test 1:" ~: (readExpr "x") ~?= (PTmVar "x"),
  "Test 2:" ~: (readExpr "abc") ~?= (PTmVar "abc"),
  "Test 3:" ~: (readExpr "z1") ~?= (PTmVar "z1")
  ]

showPTmAbsTest :: Test
showPTmAbsTest = TestList [
  "Test 1:" ~: (show $ PTmAbs "x" $ PTmVar "x") ~?= "λx.x",
  "Test 2:" ~: (show $ PTmAbs "x" $ PTmVar "y") ~?= "λx.y",
  "Test 3:" ~: (show $ PTmAbs "x" $ PTmAbs "y" $ PTmVar "z") ~?= "λx.λy.z"
  ]

parsePTmAbsTest :: Test
parsePTmAbsTest = TestList [
  "Test 1:" ~: (readExpr "λx.x") ~?= (PTmAbs "x" $ PTmVar "x"),
  "Test 2:" ~: (readExpr "λx.λy.x") ~?= (PTmAbs "x" $ PTmAbs "y" $ PTmVar "x")
  ]

showPTmAppTest :: Test
showPTmAppTest = TestList [
  "Test 1:" ~: (show $ PTmApp (PTmVar "x") (PTmVar "y")) ~?= "(x y)",
  "Test 2:" ~: (show $ PTmApp (PTmAbs "x" (PTmVar "x")) (PTmVar "y")) ~?= "(λx.x y)"
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

showTermsTest :: Test
showTermsTest = TestList [
  "Test 1:" ~: (show $ TmVar 1) ~?= "1",
  "Test 2:" ~: (show $ TmAbs $ TmVar 1) ~?= "λ.1",
  "Test 3:" ~: (show $ TmApp (TmAbs $ TmVar 0) (TmAbs $ TmAbs $ TmApp (TmVar 0) (TmVar 1)))  ~?= 
     "(λ.0 λ.λ.(0 1))"
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
    (TmApp (TmAbs $ TmVar 0) (TmAbs $ TmVar (-1)))
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
      (TmAbs $ TmAbs $ TmVar 1)]
  ]
