{-# LANGUAGE TemplateHaskell #-}
-- | HUnit tests for the 'TypeChecker' module.

module LucretiaTest.TypeCheckerTest     ( main, tests ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Test.Framework as F    ( Test )
import Test.Framework                   ( defaultMain, testGroup )
import Test.HUnit                       hiding ( defaultMain )

import Util.Debug
import Util.HUnit                       ( assertEqualShowingDiff, hUnitTestsToFrameworkTests )
import Util.VariableName                ( nv )

import Lucretia.Language.Definitions
import Lucretia.TypeChecker             ( typeProgramme, typeBlock )
import Lucretia.Language.Syntax
import Lucretia.Language.Types


main :: IO ()
main = defaultMain tests

tests :: [F.Test]
tests = hUnitTestsToFrameworkTests outputTypeTests

type OutputTestDatum = ((String, Block), String)

outputTypeTests :: [Test]
outputTypeTests = map (uncurry map_to_ATest) outputTypeTestsData
 where 
  map_to_ATest :: (String, Block) -> String -> Test
  map_to_ATest (bName, b) expectedType = TestLabel bName $ TestCase $ assertEqualShowingDiff
    ("For programme " ++ show b ++ ":")
    expectedType
    (showProgrammeType $ typeProgramme b)

outputTypeTestsData :: [OutputTestDatum]
outputTypeTestsData =
  [ ($(nv 'bSetVar_x), "X with Constraints: [Env < {x: X}, X < int]")
  , ($(nv 'bSetVar_x__Get_x), "X with Constraints: [Env < {x: X}, X < int]")
  , ($(nv 'bSetVar_x_to_x), "X with Constraints: [Env < {x: X}, X < int]")
  , ($(nv 'bSetVar_xy__Get_y), "Y with Constraints: [Env < {x: X, y: Y}, X < int, Y < string]")
  , ($(nv 'bGetUndefinedVar), "Error: Inside the main programme body a variable was referenced which may be undefined. The preconstraints were: [Env < {x: X}]")
  , ($(nv 'bGetUndefinedVar2), "Error: Inside the main programme body a variable was referenced which may be undefined. The preconstraints were: [Env < {y: Y}]")
  , ($(nv 'bNew), "X with Constraints: [Env < {}, X < {}]")
  , ($(nv 'bGetUndefinedAttr), "Error: Inside the main programme body a variable was referenced which may be undefined. The preconstraints were: [Env < {}, X < {a: Y}]")
  , ($(nv 'bGetAttr_noVar), "Error: Inside the main programme body a variable was referenced which may be undefined. The preconstraints were: [Env < {x: Y}, Y < {a: X}]")
  , ($(nv 'bGetAttr_varNotRec), "Error: Type: int should be weaker (have less possible types) then: {a: Y}")
  , ($(nv 'bSetAttr_xa), "Y with Constraints: [Env < {x: X}, X < {a: Y}, Y < int]")
  , ($(nv 'bSetAttr_xab), "A with Constraints: [A < string, Env < {x: X}, X < {a: Y, b: A}, Y < int]")
  , ($(nv 'bSetAttr_xa__Get_xa), "Y with Constraints: [Env < {x: X}, X < {a: Y}, Y < int]")
  , ($(nv 'bSetAttr_xab__Get_xb), "A with Constraints: [A < string, Env < {x: X}, X < {a: Y, b: A}, Y < int]")
  , ($(nv 'bSetAttr_xa__Set_xa_to_yb), "Y with Constraints: [A < {b: Y}, Env < {x: X, y: A}, X < {a: Y}, Y < int]")
  , ($(nv 'bSetAttr_xa__Set_xa_to_yb__Get_yb), "Y with Constraints: [A < {b: Y}, Env < {x: X, y: A}, X < {a: Y}, Y < int]")
  , ($(nv 'bSetAttr_xa__Set_xa_to_yb__Set_xa__Get_yb), "Y with Constraints: [A < {b: Y}, E < string, Env < {x: X, y: A}, X < {a: E}, Y < int]")
  , ($(nv 'bSetAttr_xa__Set_x_to_y__Set_xa__Get_ya), "Z with Constraints: [Env < {x: X, y: X}, X < {a: Z}, Z < int]")
  , ($(nv 'bFun_identity), "Y with Constraints: [Env < {identity: Y}, Y < func (Ax) [] -> Ax []]")
  , ($(nv 'bFun_identitySetFields), "C with Constraints: [C < func (Ar, Ax, Ay) [Ar < {}] -> Ar [Ar < {a: Ax, b: Ay}], Env < {identitySetFields: C}]")
  , ($(nv 'bFun_withSignature_identity), "Y with Constraints: [Env < {identity: Y}, Y < func (X) [] -> X []]")
  , ($(nv 'bFun_withSignature_identitySetFields), "C with Constraints: [C < func (R, X, Y) [R < {}] -> R [R < {a: X, b: Y}], Env < {identitySetFields: C}]")
  , ($(nv 'bFun_withSignature_tooStrongPre), "Error: Constraints: [] should be weaker or equal to [X < {a: X}]")
  , ($(nv 'bFun_withSignature_tooStrongPost), "Error: Constraints: [] should be weaker or equal to [X < {a: Y}]")
  , ($(nv 'bFun_identityNested), "Error: Please declare signature for the function identity. Infering type of a function passed as a parameter to another function (higher order function type inference) is not supported yet.")
  , ($(nv 'bFun_withSignature_identityNested), "Y with Constraints: [Env < {identityNested: Y}, Y < func (X, Identity) [Identity < func (X) [] -> X []] -> X [Identity < func (X) [] -> X []]]")
  , ($(nv 'bCall_identity), "Z with Constraints: [Env < {i: Z, identity: Y}, Y < func (Ax) [] -> Ax [], Z < int]")
  , ($(nv 'bCall_identitySetFields), "D with Constraints: [C < func (Ar, Ax, Ay) [Ar < {}] -> Ar [Ar < {a: Ax, b: Ay}], D < {a: E, b: F}, E < int, Env < {i: E, identitySetFields: C, s: F, x: D}, F < string]")
  , ($(nv 'bCall_withSignature_identityNested), "B with Constraints: [A < func (X) [] -> X [], B < int, Env < {i: B, identity: A, identityNested: Y}, Y < func (X, Identity) [Identity < func (X) [] -> X []] -> X [Identity < func (X) [] -> X []]]")
  , ($(nv 'bFun_recursive), "Z with Constraints: [Env < {f: Z}, Z < func (F) [F < func (F)  -> I ] -> I [F < func (F)  -> I , I < int]]")
  , ($(nv 'bCall_recursive), "B with Constraints: [B < int, Env < {f: Z}, Z < func (F) [F < func (F)  -> I ] -> I [F < func (F)  -> I , I < int]]")
  , ($(nv 'bFun_recursive_withParams), "Z with Constraints: [Env < {f: Z}, Z < func (F, I) [F < func (F, I)  -> I ] -> I [F < func (F, I)  -> I , I < int]]")
  , ($(nv 'bCall_recursive_withParams), "A with Constraints: [A < int, Env < {f: Z, i: A}, Z < func (F, I) [F < func (F, I)  -> I ] -> I [F < func (F, I)  -> I , I < int]]")
  , ($(nv 'bIf_doubleObjectCreation), "E with Constraints: [E < func (Acond) [Acond < bool] -> X [Acond < bool, X < {z: Z}, Z < {}], Env < {f: E}]")
  , ($(nv 'bIf_mergeOfPreviouslyCreatedObjects), "Error: There are multiple variables that should be renamed to Y. Error occured while tried to get renaming from: [A < bool, Env < {cond: A, env: X, x: Y, y: Z}, X < {z: Z}, Y < {}, Z < {}] to: [A < bool, Env < {cond: A, env: X, x: Y, y: Z}, X < {z: Y}, Y < {}, Z < {}]")
  , ($(nv 'bIf_attrUndefinedInThen), "C with Constraints: [C < func (Acond) [Acond < bool] -> X [Acond < bool, X < {optional z: Z}, Z < {}], Env < {f: C}]")
  , ($(nv 'bIf_attrUndefinedInElse), "C with Constraints: [C < func (Acond) [Acond < bool] -> X [Acond < bool, X < {optional z: Z}, Z < {}], Env < {f: C}]")
  , ($(nv 'bIf_varUndefinedInThen), "undefinedId with Constraints: [Env < {cond: X, optional x: Z}, X < bool, Z < {}]")
  , ($(nv 'bIf_varUndefinedInThen_returnVar), "Error: Possibly undefined variable was referenced. Cannot merge a fresh type pointer (i.e. created inside an if instruction) with a stale type pointer (i.e. one that should be created before the if instruction, to make sure that the referenced variable is defined).")
  , ($(nv 'bIf_preConstraints), "E with Constraints: [E < func (Acond, Ax) [Acond < bool, Ax < {a: Y}] -> undefinedId [A < int, Acond < bool, Ax < {a: Y}], Env < {f: E}]")
  , ($(nv 'bIf_varDefinedInBoth_inFunction), "B with Constraints: [B < func (Acond, Ay) [Acond < bool] -> Y [Acond < bool, Y < {}], Env < {f: B}]")
  , ($(nv 'bGetUndefinedVar_inFunction), "Error: Inside a function body a variable was referenced which may be undefined and is not in the function parameters.")
  , ($(nv 'bIf_varUndefinedInThen_inFunction), "Error: Possibly undefined variable was referenced. Cannot merge a fresh type pointer (i.e. created inside an if instruction) with a stale type pointer (i.e. one that should be created before the if instruction, to make sure that the referenced variable is defined).")
  , ($(nv 'bIf_reassignInOneBranchWithNew), "Error: Cannot merge type pointers from 'then' and 'else' branches of an 'if' instruction. Cannot merge fresh type pointer (i.e. created in a branch) with a stale type pointer (i.e. created before the branch). Only type pointers freshly created in both branches can be merged (i.e. one created in 'then', the other in 'else').")
  , ($(nv 'bIf_reassignInOneBranchWithNewCreatedOutsideOfIf), "Error: There are multiple variables that should be renamed from Y. Error occured while tried to get renaming from: [Env < {cond: Z, x: Y, y: Y}, X < {}, Y < {}, Z < bool] to: [Env < {cond: Z, x: X, y: Y}, X < {}, Y < {}, Z < bool]")
  , ($(nv 'bIf_reassignInBothBranchesWithNew), "A with Constraints: [A < {}, Env < {cond: Y, x: A}, X < {}, Y < bool]")
  , ($(nv 'bIf_reassignInOneBranchWithTheSameVar), "X with Constraints: [Env < {cond: Y, x: X}, X < {}, Y < bool]")
  , ($(nv 'bIf_reassignInOneBranchWithNew_inFunction), "Error: Possibly undefined variable was referenced. Cannot merge a fresh type pointer (i.e. created inside an if instruction) with a stale type pointer (i.e. one that should be created before the if instruction, to make sure that the referenced variable is defined).")
  , ($(nv 'bIf_reassignInOneBranchWithNewCreatedOutsideOfIf_inFunction), "Error: Possibly undefined variable was referenced. Cannot merge a fresh type pointer (i.e. created inside an if instruction) with a stale type pointer (i.e. one that should be created before the if instruction, to make sure that the referenced variable is defined).")
  , ($(nv 'bIf_reassignInBothBranchesWithNew_inFunction), "G with Constraints: [C < func (Ax) [] -> Z [X < bool, Z < {}], D < {}, Env < {f: C, xx: D}, F < bool, G < {}]")
  , ($(nv 'bIf_reassignInOneBranchWithTheSameVar_inFunction), "C with Constraints: [B < func (Ax) [] -> Ax [X < bool], C < {}, E < bool, Env < {f: B, xx: C}]")
  , ($(nv 'bIfHasAttr_attributeUndefined), "Error: Inside the main programme body a variable was referenced which may be undefined. The preconstraints were: [Env < {}, X < {optional a: Z}]")
  , ($(nv 'bIfHasAttr_attributeDefined), "undefinedId with Constraints: [Env < {x: X}, X < {optional a: Y}, Y < int]")
  , ($(nv 'bIfHasAttr_attributeMaybeDefined_usedInThen), "undefinedId with Constraints: [A < int, Env < {cond: Y, x: X}, X < {optional a: A, optional b: A}, Y < bool]")
  , ($(nv 'bIfHasAttr_attributeMaybeDefined_usedInElse), "Error: Attribute is required but it was not defined.")
  -- , ($(nv '), "C")
  ]

--bSetVar_x, bSetVar_xGet_x, bSetVar_xyGet_y, bGetUndefinedVar, bNew :: Block

cInt = EInt 42
cString = EString "hello"

bSetVar_x =
  [ SetVar "x" cInt
  ]
bSetVar_x__Get_x =
  [ SetVar "x" cInt
  , Return $ EGetVar "x"
  ]
bSetVar_x_to_x =
  [ SetVar "x" cInt
  , SetVar "x" $ EGetVar "x"
  ]
bSetVar_xy__Get_y =
  [ SetVar "x" cInt
  , SetVar "y" cString
  , Return $ EGetVar "y"
  ]
bGetUndefinedVar = 
  [ Return $ EGetVar "x"
  ]
bGetUndefinedVar2 = 
  [ SetVar "x" cInt
  , Return $ EGetVar "y"
  ]
bNew =
  [ Return ENew
  ]
bGetUndefinedAttr = 
  [ SetVar "x" ENew
  , Return $ EGetAttr "x" "a"
  ]
bGetAttr_noVar =
  [ Return $ EGetAttr "x" "a"
  ]
bGetAttr_varNotRec =
  [ SetVar "x" cInt
  , Return $ EGetAttr "x" "a"
  ]
bSetAttr_xa =
  [ SetVar "x" ENew
  , SetAttr "x" "a" cInt
  ]
bSetAttr_xab =
  [ SetVar "x" ENew
  , SetAttr "x" "a" cInt
  , SetAttr "x" "b" cString
  ]
bSetAttr_xa__Get_xa =
  [ SetVar "x" ENew
  , SetAttr "x" "a" cInt
  , Return $ EGetAttr "x" "a"
  ]
bSetAttr_xab__Get_xb =
  [ SetVar "x" ENew
  , SetAttr "x" "a" cInt
  , SetAttr "x" "b" cString
  , Return $ EGetAttr "x" "b"
  ]
bSetAttr_xa__Set_xa_to_yb =
  [ SetVar "x" ENew
  , SetAttr "x" "a" cInt
  , SetVar "y" ENew
  , SetAttr "y" "b" $ EGetAttr "x" "a"
  ]
bSetAttr_xa__Set_xa_to_yb__Get_yb =
  bSetAttr_xa__Set_xa_to_yb ++
  [ Return $ EGetAttr "y" "b"
  ]
bSetAttr_xa__Set_xa_to_yb__Set_xa__Get_yb =
  bSetAttr_xa__Set_xa_to_yb ++
  [ SetAttr "x" "a" cString
  , Return $ EGetAttr "y" "b"
  ]
bSetAttr_xa__Set_x_to_y__Set_xa__Get_ya =
  [ SetVar "x" ENew
  , SetVar "y" $ EGetVar "x"
  , SetAttr "x" "a" cInt
  , Return $ EGetAttr "y" "a"
  ]
bFun_identity =
  [ SetVar "identity" $
      EFunDef ["x"] Nothing $
      [ Return $ EGetVar "x"
      ]
  ]
bFun_identitySetFields =
  [ SetVar "identitySetFields" $
      EFunDef ["r", "x", "y"] Nothing $
      [ SetAttr "r" "a" $ EGetVar "x"
      , SetAttr "r" "b" $ EGetVar "y"
      , Return $ EGetVar "r"
      ]
  ]
bFun_withSignature_identity =
  [ SetVar "identity" $
      EFunDef ["x"]
      (Just $ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty Map.empty))
      [ Return $ EGetVar "x"
      ]
  ]
bFun_withSignature_identitySetFields =
  [ SetVar "identitySetFields" $
      EFunDef ["r", "x", "y"]
      (Just $ TFunSingle ["R", "X", "Y"] "R"
        (DeclaredPP $ PrePost
          (Map.fromList [ ("R", tOrEmptyRec)
                        ])
          (Map.fromList [ ("R", tOrFromTRec $ Map.fromList [ ("a", (Required "X"))
                                                           , ("b", (Required "Y"))
                                                           ])
                        ])
        )
      )
      [ SetAttr "r" "a" $ EGetVar "x"
      , SetAttr "r" "b" $ EGetVar "y"
      , Return $ EGetVar "r"
      ]
  ]
bFun_withSignature_tooStrongPre =
  [ SetVar "f" $
      EFunDef ["x"]
      (Just $ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty Map.empty))
      [ Return $ EGetAttr "x" "a"
      ]
  ]
bFun_withSignature_tooStrongPost =
  [ SetVar "identity" $
      EFunDef ["x"]
      (Just $ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty (Map.singleton "X" $ tOrSingletonRec "a" "Y")))
      [ Return $ EGetVar "x"
      ]
  ]
bFun_identityNested =
  [ SetVar "identityNested" $
      EFunDef ["x", "identity"]
      Nothing
      [ Return $ EFunCall "identity" ["x"]
      ]
  ]
bFun_withSignature_identityNested =
  [ SetVar "identityNested" $
      EFunDef ["x", "identity"]
      (Just $ TFunSingle ["X", "Identity"] "X"
        (DeclaredPP $ PrePost
          (Map.fromList [ ("Identity", tOrFromTFunSingle $ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty Map.empty)) ])
          (Map.fromList [ ("Identity", tOrFromTFunSingle $ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty Map.empty)) ])
        )
      )
      [ Return $ EFunCall "identity" ["x"]
      ]
  ]
bCall_identity =
  bFun_identity ++
  [ SetVar "i" cInt
  , Return $ EFunCall "identity" ["i"]
  ]
bCall_identitySetFields =
  bFun_identitySetFields ++
  [ SetVar "x" ENew
  , SetVar "i" cInt
  , SetVar "s" cString
  , Return $ EFunCall "identitySetFields" ["x", "i", "s"]
  ]
bCall_withSignature_identityNested =
  bFun_withSignature_identityNested ++
  bFun_identity ++
  [ SetVar "i" cInt
  , Return $ EFunCall "identityNested" ["i", "identity"]
  ]
preRecursive = Map.fromList
  [ ("F", tOrFromTFunSingle $ TFunSingle ["F"] "I" InheritedPP)
  ]
postRecursive = Map.fromList
  [ ("F", tOrFromTFunSingle $ TFunSingle ["F"] "I" InheritedPP)
  , ("I", tOrPrimitive KInt)
  ]
bFun_recursive =
  [ SetVar "f" $ EFunDef ["f"]
    (Just $ TFunSingle ["F"] "I"
      (DeclaredPP $ PrePost preRecursive postRecursive)
    )
    [ Return $ EFunCall "f" ["f"]
    ]
  ]
bCall_recursive =
  bFun_recursive ++
  [ Return $ EFunCall "f" ["f"]
  ]
preRecursive_withParams = Map.fromList
  [ ("F", tOrFromTFunSingle $ TFunSingle ["F", "I"] "I" InheritedPP)
  ]
postRecursive_withParams = Map.fromList
  [ ("F", tOrFromTFunSingle $ TFunSingle ["F", "I"] "I" InheritedPP)
  , ("I", tOrPrimitive KInt)
  ]
bFun_recursive_withParams =
  [ SetVar "f" $ EFunDef ["f", "i"]
    (Just $ TFunSingle ["F", "I"] "I"
      (DeclaredPP $ PrePost preRecursive_withParams postRecursive_withParams)
    )
    [ Return $ EFunCall "f" ["f", "i"]
    ]
  ]
bCall_recursive_withParams =
  bFun_recursive_withParams ++
  [ SetVar "i" cInt
  , Return $ EFunCall "f" ["f", "i"]
  ]
bIf_doubleObjectCreation =
  [ SetVar "f" $ EFunDef ["cond"] Nothing
    [ SetVar "env" ENew
    , If "cond"
      -- then
      [ SetAttr "env" "z" ENew ]
      -- else
      [ SetAttr "env" "z" ENew ]
    , Return $ EGetVar "env"
    ]
  ]
bIf_mergeOfPreviouslyCreatedObjects =
  [ SetVar "f" $ EFunDef ["cond"] Nothing
    [ SetVar "env" ENew
    , SetVar "x"   ENew
    , SetVar "y"   ENew
    , If "cond"
      -- then
      [ SetAttr "env" "z" $ EGetVar "x" ]
      -- else
      [ SetAttr "env" "z" $ EGetVar "y" ]
    , Return $ EGetVar "env"
    ]
  ]
bIf_attrUndefinedInThen =
  [ SetVar "f" $ EFunDef ["cond"] Nothing
    [ SetVar "env" ENew
    , If "cond"
      -- then
      [ ]
      -- else
      [ SetAttr "env" "z" ENew ]
    , Return $ EGetVar "env"
    ]
  ]
bIf_attrUndefinedInElse =
  [ SetVar "f" $ EFunDef ["cond"] Nothing
    [ SetVar "env" ENew
    , If "cond"
      -- then
      [ SetAttr "env" "z" ENew ]
      -- else
      [ ]
    , Return $ EGetVar "env"
    ]
  ]
bIf_varUndefinedInThen =
    [ SetVar "cond" (EBool True)
    , If "cond"
      -- then
      [ ]
      -- else
      [ SetVar "x" ENew ]
    ]
bIf_varUndefinedInThen_returnVar =
  bIf_varUndefinedInThen ++
  [ Return $ EGetVar "x"
  ]
bIf_varUndefinedInElse =
  [ SetVar "f" $ EFunDef ["cond"] Nothing
    [ SetVar "env" ENew
    , If "cond"
      -- then
      [ SetAttr "env" "z" ENew ]
      -- else
      [ ]
    , Return $ EGetVar "env"
    ]
  ]
bIf_preConstraints =
  [ SetVar "f" $ EFunDef ["cond", "x"] Nothing
    [ If "cond"
      -- then
      [ Return $ EGetAttr "x" "a"
      , SetVar "x" cInt
      ]
      -- else
      [ Return $ EGetAttr "x" "a"
      , SetVar "x" cInt
      ]
    ]
  ]
bIf_varDefinedInBoth_inFunction =
  [ SetVar "f" $ EFunDef ["cond", "y"] Nothing
    [ If "cond"
      -- then
      [ SetVar "x" ENew ]
      -- else
      [ SetVar "x" ENew ]
    , Return $ EGetVar "x"
    ]
  ]
bGetUndefinedVar_inFunction =
  [ SetVar "f" $ EFunDef [] Nothing
    [ Return $ EGetVar "x"
    ]
  ]
bIf_varUndefinedInThen_inFunction =
  [ SetVar "f" $ EFunDef ["cond", "y"] Nothing
    [ If "cond"
      -- then
      [ ]
      -- else
      [ SetVar "x" ENew ]
    , Return $ EGetVar "x"
    ]
  ]
bIf_reassignInOneBranchWithNew =
  [ SetVar "x" ENew
  , SetVar "cond" (EBool True)
  , If "cond" [ ] [ SetVar "x" ENew ]
  , Return $ EGetVar "x"
  ]
bIf_reassignInOneBranchWithNewCreatedOutsideOfIf =
  [ SetVar "x" ENew
  , SetVar "y" ENew
  , SetVar "cond" (EBool True)
  , If "cond" [ ] [ SetVar "x" $ EGetVar "y" ]
  , Return $ EGetVar "x"
  ]
bIf_reassignInBothBranchesWithNew =
  [ SetVar "x" ENew
  , SetVar "cond" (EBool True)
  , If "cond" [ SetVar "x" ENew ] [ SetVar "x" ENew ]
  , Return $ EGetVar "x"
  ]
bIf_reassignInOneBranchWithTheSameVar =
  [ SetVar "x" ENew
  , SetVar "cond" (EBool True)
  , If "cond" [ ] [ SetVar "x" $ EGetVar "x" ]
  , Return $ EGetVar "x"
  ]
bIf_reassignInOneBranchWithNew_inFunction =
  [ SetVar "f" $ EFunDef ["x"] Nothing
    [ SetVar "cond" (EBool True)
    , If "cond" [ ] [ SetVar "x" ENew ]
    , Return $ EGetVar "x"
    ]
  , SetVar "xx" ENew
  , Return $ EFunCall "f" ["xx"]
  ]
bIf_reassignInOneBranchWithNewCreatedOutsideOfIf_inFunction =
  [ SetVar "f" $ EFunDef ["x"] Nothing
    [ SetVar "cond" (EBool True)
    , SetVar "y" ENew
    , If "cond" [ ] [ SetVar "x" $ EGetVar "y" ]
    , Return $ EGetVar "x"
    ]
  , SetVar "xx" ENew
  , Return $ EFunCall "f" ["xx"]
  ]
bIf_reassignInBothBranchesWithNew_inFunction =
  [ SetVar "f" $ EFunDef ["x"] Nothing
    [ SetVar "cond" (EBool True)
    , If "cond" [ SetVar "x" ENew ] [ SetVar "x" ENew ]
    , Return $ EGetVar "x"
    ]
  , SetVar "xx" ENew
  , Return $ EFunCall "f" ["xx"]
  ]
bIf_reassignInOneBranchWithTheSameVar_inFunction =
  [ SetVar "f" $ EFunDef ["x"] Nothing
    [ SetVar "cond" (EBool True)
    , If "cond" [ ] [ SetVar "x" $ EGetVar "x" ]
    , Return $ EGetVar "x"
    ]
  , SetVar "xx" ENew
  , Return $ EFunCall "f" ["xx"]
  ]
bIfHasAttr_attributeUndefined =
  [ SetVar "x" ENew
  , IfHasAttr "x" "a" [ ] [ ]
  ]
bIfHasAttr_attributeDefined =
  -- x = new
  -- x.a = 42
  -- if x hasattr a:
  --   pass
  -- else:
  --   pass
  --
  -- [Env < {x: X}, X < {optional a: Y}, Y < int]
  [ SetVar "x" ENew
  , SetAttr "x" "a" cInt
  , IfHasAttr "x" "a" [ ] [ ]
  ]
bIfHasAttr_attributeMaybeDefined_usedInThen =
  [ SetVar "x" ENew
  , SetVar "cond" (EBool True)
  , If "cond" [ ] [ SetAttr "x" "a" cInt ]
  , IfHasAttr "x" "a" [ SetAttr "x" "b" $ EGetAttr "x" "a" ] [ ]
  ]
bIfHasAttr_attributeMaybeDefined_usedInElse =
  [ SetVar "x" ENew
  , SetVar "cond" (EBool True)
  , If "cond" [ ] [ SetAttr "x" "a" cInt ]
  , IfHasAttr "x" "a" [ ] [ SetAttr "x" "b" $ EGetAttr "x" "a" ]
  ]
