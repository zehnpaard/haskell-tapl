module ShowTermsTest (showTermsTest) where

import Test.HUnit

import Types

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


