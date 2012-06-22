--module Test where

--import qualified Lucretia.Test(tests)
import qualified Lucretia.TypeChecker.Test(tests)
import Test.HUnit(runTestTT, Test(TestList))

main :: IO ()
main = do
  legend
  runTestTT $ TestList $ Lucretia.TypeChecker.Test.tests
  return ()

legend :: IO ()
legend = putStrLn "Test results:"

