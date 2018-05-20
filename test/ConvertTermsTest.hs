module ConvertTermsTest (convertTermsTest) where

import Test.HUnit

import Types
import DeBruijn

convertTermsTest :: Test
convertTermsTest = TestList [
    "ConvertTermsTest 1:" ~: 
       (convertTerm [] (PTmVar "x")) ~?= 
       (TmVar (-1))
  , convertAbsTest
  , convertAppTest
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

convertAbsTest :: Test
convertAbsTest = TestList [
    "ConvertAbsTest 1:" ~: 
       (convertTerm [] (PTmAbs "x" TpBool $ PTmVar "x")) ~?= 
       (TmAbs TpBool $ TmVar 0)
  , "ConvertAbsTest 2:" ~: 
       (convertTerm [] (PTmAbs "x" TpBool $ PTmVar "y")) ~?= 
       (TmAbs TpBool $ TmVar (-1))
  , "ConvertAbsTest 3:" ~: 
       (convertTerm [] (PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmVar "y")) ~?= 
       (TmAbs TpBool $ TmAbs TpBool $ TmVar 0)
  , "ConvertAbsTest 4:" ~: 
       (convertTerm [] (PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmVar "x")) ~?= 
       (TmAbs TpBool $ TmAbs TpBool $ TmVar 1)
  , "ConvertAbsTest 5:" ~: 
       (convertTerm [] (PTmAbs "y" TpBool $ PTmAbs "x" TpBool $ PTmVar "y")) ~?= 
       (TmAbs TpBool $ TmAbs TpBool $ TmVar 1)
  ]

convertAppTest :: Test
convertAppTest = TestList [
    "ConvertAppTest 1:" ~: 
       (convertTerm [] (PTmApp (PTmVar "x") (PTmVar "y"))) ~?= 
       (TmApp (TmVar (-1)) (TmVar (-1)))
  , "ConvertAppTest 2:" ~: 
       (convertTerm [] (PTmApp (PTmAbs "x" TpBool $ PTmVar "x") 
                               (PTmAbs "x" TpBool $ PTmAbs "y" TpBool $ PTmVar "x"))) ~?=
       (TmApp (TmAbs TpBool $ TmVar 0) 
              (TmAbs TpBool $ TmAbs TpBool $ TmVar 1))
  , "ConvertAppTest 3:" ~: 
       (convertTerm [] (PTmApp (PTmAbs "x" TpBool $ PTmVar "x") 
                               (PTmAbs "y" TpBool $ PTmVar "x"))) ~?=
       (TmApp (TmAbs TpBool $ TmVar 0) 
              (TmAbs TpBool $ TmVar (-1)))
  ]
