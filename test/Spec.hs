import Test.HUnit

import ShowPTermsTest
import ParsePTermsTest
import ShowTermsTest
import ConvertTermsTest
import ShowTmTypesTest
import TypeCheckTest
import EvalTest

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


