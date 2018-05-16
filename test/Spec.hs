import Test.HUnit

import Types

main :: IO ()
main = do
  runTestTT $ TestList [showTmVarTest]
  return ()

showTmVarTest :: Test
showTmVarTest = TestList [
  "Test 1:" ~: (show $ TmVar "test") ~?= "test",
  "Test 2:" ~: (show $ TmVar "a") ~?= "a",
  "Test 3:" ~: (show $ TmVar "") ~?= ""]
