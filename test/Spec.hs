import Test.HUnit

import ShowPTermsTest
import ParsePTermsTest
import ShowTmTypesTest
import TypeCheckTest

import Types
import Parser
import DeBruijn
import Evaluate

main :: IO ()
main = do
  runTestTT $ TestList [
      showPTermsTest
    , parsePTermsTest
    , showTermsTest
    , convertTermsTest
    , evalTest
    , showTmTypesTest
    , typeCheckTest
    ]
  return ()

showTermsTest :: Test
showTermsTest = TestList [
    "ShowTermsTest 1:" ~: (show $ TmVar 1) ~?= "1"
  , "ShowTermsTest 2:" ~: 
       (show $ TmAbs TpBool $ TmVar 1) ~?= 
       "λ:Bool.1"
  , "ShowTermsTest 3:" ~: 
       (show $ TmApp (TmAbs TpBool $ TmVar 0) (TmAbs TpBool $ TmAbs TpBool $ TmApp (TmVar 0) (TmVar 1)))  ~?= 
       "(λ:Bool.0 λ:Bool.λ:Bool.(0 1))"
  , "ShowTermsTest 4:" ~: (show TmTrue) ~?= "true"
  , "ShowTermsTest 5:" ~: (show TmFalse) ~?= "false"
  , "ShowTermsTest 6:" ~: 
       (show $ TmIf TmTrue TmFalse TmFalse) ~?= 
       "(if true then false else false)"
  ]

convertTermsTest :: Test
convertTermsTest = TestList [
    "ConvertTermsTest 1:" ~: 
       (convertTerm [] (PTmVar "x")) ~?= 
       (TmVar (-1))
  , "ConvertTermsTest 2:" ~: 
       (convertTerm [] (PTmAbs "x" TpBool $ PTmVar "x")) ~?= 
       (TmAbs TpBool $ TmVar 0)
  , "ConvertTermsTest 3:" ~: 
       (convertTerm [] (PTmAbs "x" TpBool $ PTmVar "y")) ~?= 
       (TmAbs TpBool $ TmVar (-1))
  , "ConvertTermsTest 4:" ~: 
       (convertTerm [] (PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmVar "y")) ~?= 
       (TmAbs TpBool $ TmAbs TpBool $ TmVar 0)
  , "ConvertTermsTest 5:" ~: 
       (convertTerm [] (PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmVar "x")) ~?= 
       (TmAbs TpBool $ TmAbs TpBool $ TmVar 1)
  , "ConvertTermsTest 6:" ~: 
       (convertTerm [] (PTmAbs "y" TpBool $ PTmAbs "x" TpBool $ PTmVar "y")) ~?= 
       (TmAbs TpBool $ TmAbs TpBool $ TmVar 1)
  , "ConvertTermsTest 7:" ~: 
       (convertTerm [] (PTmApp (PTmVar "x") (PTmVar "y"))) ~?= 
       (TmApp (TmVar (-1)) (TmVar (-1)))
  , "ConvertTermsTest 8:" ~: 
       (convertTerm [] (PTmApp (PTmAbs "x" TpBool $ PTmVar "x") 
                               (PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmVar "x"))) ~?=
       (TmApp (TmAbs TpBool $ TmVar 0) 
              (TmAbs TpBool $ TmAbs TpBool $ TmVar 1))
  , "ConvertTermsTest 9:" ~: 
       (convertTerm [] (PTmApp (PTmAbs "x" TpBool $ PTmVar "x") 
                               (PTmAbs "y" TpBool $ PTmVar "x"))) ~?=
       (TmApp (TmAbs TpBool $ TmVar 0) 
              (TmAbs TpBool $ TmVar (-1)))
  , "ConvertTermsTest 10:" ~: (convertTerm [] PTmTrue) ~?= TmTrue
  , "ConvertTermsTest 11:" ~: (convertTerm [] PTmFalse) ~?= TmFalse
  , "ConvertTermsTest 12:" ~: 
       (convertTerm [] (PTmIf PTmTrue 
                              PTmFalse 
                              (PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmApp (PTmVar "y") 
                                                                              (PTmVar "x")))) ~?=
       (TmIf TmTrue 
             TmFalse $ 
             TmAbs TpBool $ TmAbs TpBool $ TmApp (TmVar 0) 
                                                 (TmVar 1))
  ]

evalTest :: Test
evalTest = TestList [
    "EvalTest 1:" ~: (substitute (TmVar 1) (TmVar 0)) ~?= (TmVar 1)
  , "EvalTest 2:" ~: 
       (substitute (TmVar 1) (TmAbs TpBool $ TmVar 0)) ~?= 
       (TmAbs TpBool $ TmVar 0)
  , "EvalTest 3:" ~: 
       (substitute (TmVar 3) (TmAbs TpBool $ TmVar 1)) ~?= 
       (TmAbs TpBool $ TmVar 3)
  , "EvalTest 4:" ~: 
       (eval (TmAbs TpBool $ TmVar 1)) ~?= 
       [(TmAbs TpBool $ TmVar 1)]
  , "EvalTest 5:" ~: 
       (eval (TmApp (TmAbs TpBool $ TmAbs TpBool $ TmVar 1) 
                    (TmAbs TpBool $ TmVar 0))) ~?= 
       [(TmApp (TmAbs TpBool $ TmAbs TpBool $ TmVar 1) 
               (TmAbs TpBool $ TmVar 0)),
        (TmAbs TpBool $ TmAbs TpBool $ TmVar 0)]
  , "EvalTest 6:" ~: 
       (eval (TmApp (TmAbs TpBool $ TmVar 0) 
             (TmAbs TpBool $ TmAbs TpBool $ TmVar 1))) ~?= 
       [(TmApp (TmAbs TpBool $ TmVar 0) 
               (TmAbs TpBool $ TmAbs TpBool $ TmVar 1)),
        (TmAbs TpBool $ TmAbs TpBool $ TmVar 1)]
  , "EvalTest 7:" ~: (eval TmTrue) ~?= [TmTrue]
  , "EvalTest 8:" ~: (eval TmFalse) ~?= [TmFalse]
  , "EvalTest 9:" ~: 
       (eval (TmApp (TmAbs TpBool $ TmAbs TpBool $ TmVar 1) 
             (TmAbs TpBool $ TmFalse))) ~?= 
       [(TmApp (TmAbs TpBool $ TmAbs TpBool $ TmVar 1) 
               (TmAbs TpBool $ TmFalse)),
        (TmAbs TpBool $ TmAbs TpBool $ TmFalse)]
  , "EvalTest 10:" ~: 
       (eval (TmIf TmTrue TmFalse TmTrue)) ~?= [(TmIf TmTrue TmFalse TmTrue), TmFalse]
  , "EvalTest 11:" ~: 
       (eval (TmIf (TmApp (TmAbs TpBool $ TmVar 0) 
                          TmTrue) 
                   (TmAbs TpBool $ TmVar 0) 
                   (TmAbs TpBool $ TmFalse))) ~?=
       [(TmIf (TmApp (TmAbs TpBool $ TmVar 0) 
                     TmTrue) 
              (TmAbs TpBool $ TmVar 0) 
              (TmAbs TpBool $ TmFalse)),
        (TmIf TmTrue 
              (TmAbs TpBool $ TmVar 0) 
              (TmAbs TpBool $ TmFalse)),
        (TmAbs TpBool $ TmVar 0)]
  ]
