import Test.HUnit

main :: IO ()
main = do
  runTestTT $ TestList []
  return ()
