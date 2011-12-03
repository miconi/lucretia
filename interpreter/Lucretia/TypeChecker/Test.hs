--module Lucretia.TypeChecker.Test(tests, main) where
module Lucretia.TypeChecker.Test where

import Test.HUnit

import HUnitUtils(assertEqualShowingDiff)

import Lucretia.TypeChecker.Main(checkProg, runCheck)
import Lucretia.TypeChecker.Syntax
import Lucretia.TypeChecker.Types

tests :: [Test]
tests = outputTypeTests


outputTypeTestsData :: [(Exp, String)]
outputTypeTestsData = [
  (eInt, "Right (int,[])"),
  (ENew, "Right (X1,[X1 < {}])"),
  (ELet "foo" ENew (EVar "foo"), "Right (X1,[X1 < {}])"),
  (oneFieldRecord, "Right (X1,[X1 < {a:int}])"),
  (recordWithManyFields, "Right (X1,[X1 < {a:int, b:int, c:int}])")
  ]

outputTypeTests :: [Test]
outputTypeTests = map (uncurry mapToATest) outputTypeTestsData
 where 
  mapToATest :: Exp -> String -> Test
  mapToATest e expectedType = TestCase $ assertEqualShowingDiff
    ("For program " ++ show e ++ ":")
    expectedType
    (show $ runCheck e)


eInt :: Exp
eInt = EInt 42
eBoolTrue :: Exp
eBoolTrue = EBoolTrue
eBoolFalse :: Exp
eBoolFalse = EBoolFalse

oneFieldRecord :: Exp
oneFieldRecord = 
  ELet "foo" ENew $
  (ESet "foo" "a" $ EInt 42)

recordWithManyFields :: Exp
recordWithManyFields = 
  ELet "foo" ENew $
  ELet "_" (ESet "foo" "a" $ EInt 42) $
  ELet "_" (ESet "foo" "b" $ EInt 42) $
  ELet "_" (ESet "foo" "c" $ EInt 42) $
  EVar "foo"

typeOrExampleReturnFooA :: Exp
typeOrExampleReturnFooA =
  typeOrExample $
  EGet "foo" "a"

typeOrExample :: Exp -> Exp
typeOrExample returnExp = 
  ELet "foo" ENew $
  ELet "_"
  (EIf (EBoolTrue)
    (ESet "foo" "a" $ EInt 42)
    (ESet "foo" "a" $ EBoolTrue)
  ) $
  returnExp

typeFieldUndefinedExampleReturnFoo =
  typeFieldUndefinedExample $ 
  EVar "foo"

typeFieldUndefinedExampleTypeError =
  typeFieldUndefinedExample $ 
  EGet "foo" "a"

typeFieldUndefinedExample :: Exp -> Exp
typeFieldUndefinedExample returnExp = 
  ELet "foo" ENew $
  ELet "_"
  (EIf (EBoolTrue)
    (ESet "foo" "a" $ EInt 42)
    (ESet "foo" "b" $ EBoolTrue)
  ) $
  returnExp


