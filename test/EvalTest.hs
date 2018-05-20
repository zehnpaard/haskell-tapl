module EvalTest (evalTest) where

import Test.HUnit

import Types
import Evaluate

evalTest :: Test
evalTest = TestList [
    "EvalTest 1:" ~: (substitute (TmVar 1) (TmVar 0)) ~?= (TmVar 1)
  , "EvalTest 2:" ~: 
       (substitute (TmVar 1) (TmAbs TpBool $ TmVar 0)) ~?= 
       (TmAbs TpBool $ TmVar 0)
  , "EvalTest 3:" ~: 
       (substitute (TmVar 3) (TmAbs TpBool $ TmVar 1)) ~?= 
       (TmAbs TpBool $ TmVar 3)
  , evalAbsTest
  , evalAppTest
  , evalIfTest
  ]

evalAbsTest :: Test
evalAbsTest = TestList [
    "EvalAbsTest 1:" ~: 
       (eval (TmAbs TpBool $ TmVar 1)) ~?= 
       [(TmAbs TpBool $ TmVar 1)]
  ]

evalAppTest :: Test
evalAppTest = TestList [
    "EvalAppTest 1:" ~: 
       (eval (TmApp (TmAbs TpBool $ TmAbs TpBool $ TmVar 1) 
                    (TmAbs TpBool $ TmVar 0))) ~?= 
       [(TmApp (TmAbs TpBool $ TmAbs TpBool $ TmVar 1) 
               (TmAbs TpBool $ TmVar 0)),
        (TmAbs TpBool $ TmAbs TpBool $ TmVar 0)]
  , "EvalAppTest 2:" ~: 
       (eval (TmApp (TmAbs TpBool $ TmVar 0) 
             (TmAbs TpBool $ TmAbs TpBool $ TmVar 1))) ~?= 
       [(TmApp (TmAbs TpBool $ TmVar 0) 
               (TmAbs TpBool $ TmAbs TpBool $ TmVar 1)),
        (TmAbs TpBool $ TmAbs TpBool $ TmVar 1)]
  , "EvalAppTest 3:" ~: (eval TmTrue) ~?= [TmTrue]
  , "EvalAppTest 4:" ~: (eval TmFalse) ~?= [TmFalse]
  , "EvalAppTest 5:" ~: 
       (eval (TmApp (TmAbs TpBool $ TmAbs TpBool $ TmVar 1) 
             (TmAbs TpBool $ TmFalse))) ~?= 
       [(TmApp (TmAbs TpBool $ TmAbs TpBool $ TmVar 1) 
               (TmAbs TpBool $ TmFalse)),
        (TmAbs TpBool $ TmAbs TpBool $ TmFalse)]
  ]

evalIfTest :: Test
evalIfTest = TestList [
    "EvalIfTest 1:" ~: 
       (eval (TmIf TmTrue TmFalse TmTrue)) ~?= [(TmIf TmTrue TmFalse TmTrue), TmFalse]
  , "EvalIfTest 2:" ~: 
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
