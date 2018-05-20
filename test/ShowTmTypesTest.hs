module ShowTmTypesTest (showTmTypesTest) where

import Test.HUnit

import Types

showTmTypesTest :: Test
showTmTypesTest = TestList [
    "ShowTmTypesTest 1:" ~: (show $ TpBool) ~?= "Bool"
  , "ShowTmTypesTest 2:" ~: 
       (show $ TpArrow TpBool TpBool) ~?= 
       "Bool->Bool"
  , "ShowTmTypesTest 3:" ~: 
       (show $ TpArrow TpBool $ TpArrow TpBool TpBool) ~?= 
       "Bool->Bool->Bool"
  , "ShowTmTypesTest 4:" ~: 
       (show $ TpArrow (TpArrow TpBool TpBool) TpBool) ~?= 
       "(Bool->Bool)->Bool"
  , "ShowTmTypesTest 5:" ~: 
       (show $ TpArrow (TpArrow TpBool TpBool) $ TpArrow (TpArrow TpBool TpBool) TpBool) ~?= 
       "(Bool->Bool)->(Bool->Bool)->Bool"
  ]
