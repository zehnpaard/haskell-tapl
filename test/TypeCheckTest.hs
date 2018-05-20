module TypeCheckTest (typeCheckTest) where

import Test.HUnit

import TypeCheck
import Types

typeCheckTest :: Test
typeCheckTest = TestList [
  "TypeCheckTest 1:" ~:
      typeOf [] TmTrue ~?=
      Just TpBool,
  "TypeCheckTest 2:" ~:
      typeOf [] TmFalse ~?=
      Just TpBool,
  "TypeCheckTest 3:" ~:
      typeOf [] (TmVar 0) ~?=
      Nothing,
  typeCheckAbsTest,
  typeCheckAppTest,
  typeCheckIfTest
  ]

typeCheckAbsTest :: Test
typeCheckAbsTest = TestList [
  "TypeCheckAbsTest 1:" ~: 
      typeOf [] (TmAbs TpBool TmTrue) ~?= 
      (Just $ TpArrow TpBool TpBool),
  "TypeCheckAbsTest 2:" ~: 
      typeOf [] (TmAbs TpBool $ TmVar 0) ~?= 
      (Just $ TpArrow TpBool TpBool),
  "TypeCheckAbsTest 3:" ~: 
      typeOf [] (TmAbs TpBool $ TmVar 1) ~?= 
      Nothing,
  "TypeCheckAbsTest 4:" ~: 
      typeOf [] (TmAbs (TpArrow TpBool TpBool) $ TmVar 0) ~?= 
      (Just $ TpArrow (TpArrow TpBool TpBool) (TpArrow TpBool TpBool)),
  "TypeCheckAbsTest 5:" ~: 
      typeOf [] (TmAbs TpBool $ TmAbs TpBool $ TmVar 1) ~?= 
      (Just $ TpArrow TpBool $ TpArrow TpBool TpBool),
  "TypeCheckAbsTest 6:" ~: 
      typeOf [] (TmAbs (TpArrow TpBool TpBool) TmTrue) ~?= 
      (Just $ TpArrow (TpArrow TpBool TpBool) TpBool)
  ]

typeCheckAppTest :: Test
typeCheckAppTest = TestList [
  "TypeCheckAppTest 1:" ~:
     typeOf [] (TmApp TmTrue TmTrue) ~?=
     Nothing,
  "TypeCheckAppTest 2:" ~:
     typeOf [] (TmApp (TmAbs TpBool TmFalse) (TmAbs TpBool TmFalse)) ~?=
     Nothing,
  "TypeCheckAppTest 3:" ~:
     typeOf [] (TmApp (TmAbs TpBool TmFalse) TmTrue) ~?=
     Just TpBool,
  "TypeCheckAppTest 4:" ~:
     typeOf [] (TmApp (TmAbs (TpArrow TpBool TpBool) TmFalse) (TmAbs TpBool TmFalse)) ~?=
     Just TpBool,
  "TypeCheckAppTest 5:" ~:
     typeOf [] (TmApp (TmAbs TpBool TmTrue) (TmApp (TmAbs TpBool TmFalse) TmTrue)) ~?=
     Just TpBool,
  "TypeCheckAppTest 6:" ~:
     typeOf [] (TmApp (TmApp (TmAbs TpBool $ TmAbs TpBool $ TmVar 1) TmTrue) TmFalse) ~?=
     Just TpBool,
  "TypeCheckAppTest 7:" ~:
     typeOf [] (TmApp (TmIf TmTrue (TmAbs TpBool $ TmVar 0) (TmAbs TpBool TmTrue)) TmFalse) ~?=
     Just TpBool
  ]

typeCheckIfTest :: Test
typeCheckIfTest = TestList [
  "TypeCheckIfTest 1:" ~:
     typeOf [] (TmIf TmTrue TmFalse TmTrue) ~?=
     Just TpBool,
  "TypeCheckIfTest 2:" ~:
     typeOf [] (TmIf (TmVar 0) TmFalse TmTrue) ~?=
     Nothing, 
  "TypeCheckIfTest 3:" ~:
     typeOf [] (TmIf TmTrue (TmVar 0) TmTrue) ~?=
     Nothing, 
  "TypeCheckIfTest 4:" ~:
     typeOf [] (TmIf TmFalse TmTrue (TmVar 0)) ~?=
     Nothing, 
  "TypeCheckIfTest 5:" ~:
     typeOf [] (TmIf TmFalse (TmAbs TpBool $ TmVar 0) TmTrue) ~?=
     Nothing,
  "TypeCheckIfTest 6:" ~:
     typeOf [] (TmIf TmFalse (TmAbs TpBool $ TmVar 0) (TmAbs TpBool TmTrue)) ~?=
     (Just $ TpArrow TpBool TpBool),
  "TypeCheckIfTest 7:" ~:
     typeOf [] (TmIf TmFalse (TmApp (TmAbs TpBool $ TmVar 0) TmFalse) TmTrue) ~?=
     Just TpBool
  ]
