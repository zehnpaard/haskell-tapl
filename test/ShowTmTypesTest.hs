module ShowTmTypesTest (showTmTypesTest) where

import Test.HUnit

import Types

showTmTypesTest :: Test
showTmTypesTest = TestList [
  "Test 1:" ~: (show $ TpBool) ~?= "Bool",
  "Test 2:" ~: (show $ TpArrow TpBool TpBool) ~?= "Bool->Bool",
  "Test 3:" ~: (show $ TpArrow TpBool $ TpArrow TpBool TpBool) ~?= "Bool->Bool->Bool",
  "Test 4:" ~: (show $ TpArrow (TpArrow TpBool TpBool) TpBool) ~?= "(Bool->Bool)->Bool",
  "Test 5:" ~: (show $ TpArrow (TpArrow TpBool TpBool) $ TpArrow (TpArrow TpBool TpBool) TpBool) ~?= 
     "(Bool->Bool)->(Bool->Bool)->Bool"
  ]
