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
  , ($(nv 'bGetUndefinedAttr), "Error: Attribute is required but it was not defined.")
  , ($(nv 'bGetAttr_noVar), "Error: Inside the main programme body a variable was referenced which may be undefined. The preconstraints were: [Env < {x: Y}, Y < {a: X}]")
  , ($(nv 'bGetAttr_varNotRec), "Error: Type: int should be weaker (have less possible types) then: {a: Y}")
  , ($(nv 'bSetAttr_xa), "Z with Constraints: [Env < {x: X}, X < {a: Z}, Z < int]")
  , ($(nv 'bSetAttr_xab), "B with Constraints: [B < string, Env < {x: X}, X < {a: Z, b: B}, Z < int]")
  , ($(nv 'bSetAttr_xa__Get_xa), "Z with Constraints: [Env < {x: X}, X < {a: Z}, Z < int]")
  , ($(nv 'bSetAttr_xab__Get_xb), "B with Constraints: [B < string, Env < {x: X}, X < {a: Z, b: B}, Z < int]")
  , ($(nv 'bSetAttr_xa__Set_xa_to_yb), "Z with Constraints: [A < {b: Z}, Env < {x: X, y: A}, X < {a: Z}, Z < int]")
  , ($(nv 'bSetAttr_xa__Set_xa_to_yb__Get_yb), "Z with Constraints: [A < {b: Z}, Env < {x: X, y: A}, X < {a: Z}, Z < int]")
  , ($(nv 'bSetAttr_xa__Set_xa_to_yb__Set_xa__Get_yb), "Z with Constraints: [A < {b: Z}, Env < {x: X, y: A}, F < string, X < {a: F}, Z < int]")
  , ($(nv 'bSetAttr_xa__Set_x_to_y__Set_xa__Get_ya), "A with Constraints: [A < int, Env < {x: X, y: X}, X < {a: A}]")
  , ($(nv 'bFun_identity), "Z with Constraints: [Env < {identity: Z}, Z < func (Y) [] -> Y []]")
  , ($(nv 'bFun_identitySetFields), "F with Constraints: [Env < {identitySetFields: F}, F < func (C, D, E) [C < {}] -> C [C < {a: D, b: E}]]")
  , ($(nv 'bFun_withSignature_identity), "Z with Constraints: [Env < {identity: Z}, Z < func (Y) [] -> Y []]")
  , ($(nv 'bFun_withSignature_identitySetFields), "F with Constraints: [Env < {identitySetFields: F}, F < func (C, D, E) [C < {}] -> C [C < {a: D, b: E}]]")
  , ($(nv 'bFun_withSignature_tooStrongPre), "Error: There were following references to attributes undefined in the preconstraints in the declared signature of a function: [SX < {a: X}]")
  , ($(nv 'bFun_withSignature_tooStrongPost), "Error: The list of type pointer names should be the same in actual and declared function postconstraints: [] and [SX < {a: SY}]")
  , ($(nv 'bFun_identityNested), "Error: Please declare signature for the function identity. Infering type of a function passed as a parameter to another function (higher order function type inference) is not supported yet.")
  , ($(nv 'bFun_withSignature_identityNested), "A with Constraints: [A < func (Z, Y) [Y < func (Z) [] -> Z []] -> Z [Y < func (Z) [] -> Z []], Env < {identityNested: A}]")
  , ($(nv 'bCall_identity), "A with Constraints: [A < int, Env < {i: A, identity: Z}, Z < func (Y) [] -> Y []]")
  , ($(nv 'bCall_identitySetFields), "G with Constraints: [Env < {i: H, identitySetFields: F, s: I, x: G}, F < func (C, D, E) [C < {}] -> C [C < {a: D, b: E}], G < {a: H, b: I}, H < int, I < string]")
  , ($(nv 'bCall_withSignature_identityNested), "E with Constraints: [A < func (Z, Y) [Y < func (Z) [] -> Z []] -> Z [Y < func (Z) [] -> Z []], D < func (E) [] -> E [], E < int, Env < {i: E, identity: D, identityNested: A}]")
  , ($(nv 'bFun_recursive), "B with Constraints: [B < func (Z) [Z < func (Z)  -> A ] -> A [A < int, Z < func (Z)  -> A ], Env < {f: B}]")
  , ($(nv 'bCall_recursive), "C with Constraints: [B < func (B) [B < func (B)  -> C ] -> C [B < func (B)  -> C , C < int], C < int, Env < {f: B}]")
  , ($(nv 'bFun_recursive_withParams), "B with Constraints: [B < func (Z, A) [Z < func (Z, A)  -> A ] -> A [A < int, Z < func (Z, A)  -> A ], Env < {f: B}]")
  , ($(nv 'bCall_recursive_withParams), "C with Constraints: [B < func (B, C) [B < func (B, C)  -> C ] -> C [B < func (B, C)  -> C , C < int], C < int, Env < {f: B, i: C}]")
  , ($(nv 'bIf_doubleObjectCreation), "H with Constraints: [Env < {f: H}, H < func (F) [F < bool] -> G [E < {}, F < bool, G < {z: E}]]")
  , ($(nv 'bIf_mergeOfPreviouslyCreatedObjects), "Error: There are multiple variables that should be renamed to Y. Error occured while tried to get renaming from: [A < bool, Env < {cond: A, env: X, x: Y, y: Z}, X < {z: Z}, Y < {}, Z < {}] to: [A < bool, Env < {cond: A, env: X, x: Y, y: Z}, X < {z: Y}, Y < {}, Z < {}]")
  , ($(nv 'bIf_attrUndefinedInThen), "F with Constraints: [Env < {f: F}, F < func (D) [D < bool] -> E [C < {}, D < bool, E < {optional z: C}]]")
  , ($(nv 'bIf_attrUndefinedInElse), "F with Constraints: [Env < {f: F}, F < func (D) [D < bool] -> E [C < {}, D < bool, E < {optional z: C}]]")
  , ($(nv 'bIf_varUndefinedInThen), "undefinedId with Constraints: [Env < {cond: X, optional x: Z}, X < bool, Z < {}]")
  , ($(nv 'bIf_varUndefinedInThen_returnVar), "Error: Possibly undefined variable was referenced. Cannot merge a fresh type pointer (i.e. created inside an if instruction) with a stale type pointer (i.e. one that should be created before the if instruction, to make sure that the referenced variable is defined).")
  , ($(nv 'bIf_preConstraints), "J with Constraints: [Env < {f: J}, J < func (F, G) [F < bool, G < {a: H}] -> I [E < int, F < bool, G < {a: H}]]")
  , ($(nv 'bIf_varDefinedInBoth_inFunction), "E with Constraints: [E < func (B, C) [B < bool] -> D [B < bool, D < {}], Env < {f: E}]")
  , ($(nv 'bGetUndefinedVar_inFunction), "Error: Inside a function body a variable was referenced which may be undefined and is not in the function parameters.")
  , ($(nv 'bIf_varUndefinedInThen_inFunction), "Error: Possibly undefined variable was referenced. Cannot merge a fresh type pointer (i.e. created inside an if instruction) with a stale type pointer (i.e. one that should be created before the if instruction, to make sure that the referenced variable is defined).")
  , ($(nv 'bIf_reassignInOneBranchWithNew), "Error: Cannot merge type pointers from 'then' and 'else' branches of an 'if' instruction. Cannot merge fresh type pointer (i.e. created in a branch) with a stale type pointer (i.e. created before the branch). Only type pointers freshly created in both branches can be merged (i.e. one created in 'then', the other in 'else').")
  , ($(nv 'bIf_reassignInOneBranchWithNewCreatedOutsideOfIf), "Error: There are multiple variables that should be renamed from Y. Error occured while tried to get renaming from: [Env < {cond: Z, x: Y, y: Y}, X < {}, Y < {}, Z < bool] to: [Env < {cond: Z, x: X, y: Y}, X < {}, Y < {}, Z < bool]")
  , ($(nv 'bIf_reassignInBothBranchesWithNew), "A with Constraints: [A < {}, Env < {cond: Y, x: A}, X < {}, Y < bool]")
  , ($(nv 'bIf_reassignInOneBranchWithTheSameVar), "X with Constraints: [Env < {cond: Y, x: X}, X < {}, Y < bool]")
  , ($(nv 'bIf_reassignInOneBranchWithNew_inFunction), "Error: Possibly undefined variable was referenced. Cannot merge a fresh type pointer (i.e. created inside an if instruction) with a stale type pointer (i.e. one that should be created before the if instruction, to make sure that the referenced variable is defined).")
  , ($(nv 'bIf_reassignInOneBranchWithNewCreatedOutsideOfIf_inFunction), "Error: Possibly undefined variable was referenced. Cannot merge a fresh type pointer (i.e. created inside an if instruction) with a stale type pointer (i.e. one that should be created before the if instruction, to make sure that the referenced variable is defined).")
  , ($(nv 'bIf_reassignInBothBranchesWithNew_inFunction), "J with Constraints: [Env < {f: F, xx: G}, F < func (C) [] -> E [D < bool, E < {}], G < {}, I < bool, J < {}]")
  , ($(nv 'bIf_reassignInOneBranchWithTheSameVar_inFunction), "E with Constraints: [D < func (B) [] -> B [C < bool], E < {}, Env < {f: D, xx: E}, G < bool]")
  , ($(nv 'bIfHasAttr_attributeUndefined), "undefinedId with Constraints: [Env < {x: X}, X < {optional a: Z}] AND undefinedId with Constraints: [Env < {x: X}, X < {forbidden a}]")
  , ($(nv 'bIfHasAttr_attributeUndefined_setAttrInElse), "F with Constraints: [Env < {x: X}, F < int, X < {forbidden a, b: F}]")
  , ($(nv 'bIfHasAttr_attributeDefined), "undefinedId with Constraints: [Env < {x: X}, X < {optional a: Z}, Z < int] AND undefinedId with Constraints: [Env < {x: X}, X < {a: Z}, Z < int]")
  , ($(nv 'bIfHasAttr_attributeMaybeDefined_usedInThen), "undefinedId with Constraints: [B < int, Env < {cond: Y, x: X}, X < {optional a: B, optional b: B}, Y < bool]")
  , ($(nv 'bIfHasAttr_attributeMaybeDefined_usedInElse), "Error: No typing path has succeeded: [\"Attribute is required but it was not defined.\",\"Possibly undefined variable was referenced. Cannot merge a fresh type pointer (i.e. created inside an if instruction) with a stale type pointer (i.e. one that should be created before the if instruction, to make sure that the referenced variable is defined).\",\"Attribute is forbidden but it may have been defined.\"]")
  , ($(nv 'bFun_withSignature_intersectionExample_firstSignature), "F with Constraints: [Env < {f: F}, F < func (D, E) [D < {}, E < {a: C}] -> C [B < int, D < {a: B}, E < {a: C}]]")
  , ($(nv 'bCall_withSignature_intersectionExample_firstSignature), "J with Constraints: [Env < {f: F, xx: G, yy: H}, F < func (D, E) [D < {}, E < {a: C}] -> C [B < int, D < {a: B}, E < {a: C}], G < {a: K}, H < {a: J}, J < bool, K < int]")
  , ($(nv 'bFun_withSignature_intersectionExample_secondSignature), "D with Constraints: [D < func (C, C) [C < {}] -> B [B < int, C < {a: B}], Env < {f: D}]")
  , ($(nv 'bCall_withSignature_intersectionExample_secondSignature), "F with Constraints: [D < func (C, C) [C < {}] -> B [B < int, C < {a: B}], E < {a: F}, Env < {f: D, xx: E}, F < int]")
  , ($(nv 'bFun_withTwoSignatures_intersectionExample), "J with Constraints: [Env < {f: J}, J < func (H, I) [H < {}, I < {a: G}] -> G [F < int, H < {a: F}, I < {a: G}] and (H, H) [H < {}] -> F [F < int, H < {a: F}]]")
  , ($(nv 'bCall_withTwoSignatures_intersectionExample_firstSignature), "N with Constraints: [Env < {f: J, xx: K, yy: L}, J < func (H, I) [H < {}, I < {a: G}] -> G [F < int, H < {a: F}, I < {a: G}] and (H, H) [H < {}] -> F [F < int, H < {a: F}], K < {a: O}, L < {a: N}, N < bool, O < int]")
  , ($(nv 'bCall_withTwoSignatures_intersectionExample_secondSignature), "P with Constraints: [Env < {f: J, xx: K}, J < func (H, I) [H < {}, I < {a: G}] -> G [F < int, H < {a: F}, I < {a: G}] and (H, H) [H < {}] -> F [F < int, H < {a: F}], K < {a: P}, P < int]")
  , ($(nv 'bCall_withTwoSignatures_intersectionExample_secondSignature_badArgs), "Error: No typing path has succeeded: [\"Type: int should be weaker (have less possible types) then: {a: M}\",\"Type: int should be weaker (have less possible types) then: {}\"]")
  , ($(nv 'bFun_withTwoSignatures_tooStrongPre), "Error: There were following references to attributes undefined in the preconstraints in the declared signature of a function: [SX < {a: X}]")
  , ($(nv 'bFun_withTwoSignatures_tooWeakPost), "Error: The list of type pointer names should be the same in actual and declared function postconstraints: [SX < {a: Y}, Y < int] and [SX < {}]")
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
      EFunDef ["x"] [] $
      [ Return $ EGetVar "x"
      ]
  ]
bFun_identitySetFields =
  [ SetVar "identitySetFields" $
      EFunDef ["r", "x", "y"] [] $
      [ SetAttr "r" "a" $ EGetVar "x"
      , SetAttr "r" "b" $ EGetVar "y"
      , Return $ EGetVar "r"
      ]
  ]
bFun_withSignature_identity =
  [ SetVar "identity" $
      EFunDef ["x"]
      [ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty Map.empty)]
      [ Return $ EGetVar "x"
      ]
  ]
bFun_withSignature_identitySetFields =
  [ SetVar "identitySetFields" $
      EFunDef ["r", "x", "y"]
      [ TFunSingle ["R", "X", "Y"] "R"
        (DeclaredPP $ PrePost
          (Map.fromList [ ("R", tOrEmptyRec)
                        ])
          (Map.fromList [ ("R", tOrFromTRec $ Map.fromList [ ("a", (Required "X"))
                                                           , ("b", (Required "Y"))
                                                           ])
                        ])
        )
      ]
      [ SetAttr "r" "a" $ EGetVar "x"
      , SetAttr "r" "b" $ EGetVar "y"
      , Return $ EGetVar "r"
      ]
  ]
bFun_withSignature_tooStrongPre =
  [ SetVar "f" $
      EFunDef ["x"]
      [ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty Map.empty)]
      [ Return $ EGetAttr "x" "a"
      ]
  ]
bFun_withSignature_tooStrongPost =
  [ SetVar "identity" $
      EFunDef ["x"]
      [ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty (Map.singleton "X" $ tOrSingletonRec "a" "Y"))]
      [ Return $ EGetVar "x"
      ]
  ]
bFun_identityNested =
  [ SetVar "identityNested" $
      EFunDef ["x", "identity"]
      []
      [ Return $ EFunCall "identity" ["x"]
      ]
  ]
bFun_withSignature_identityNested =
  [ SetVar "identityNested" $
      EFunDef ["x", "identity"]
      [ TFunSingle ["X", "Identity"] "X"
        (DeclaredPP $ PrePost
          (Map.fromList [ ("Identity", tOrFromTFunSingle $ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty Map.empty)) ])
          (Map.fromList [ ("Identity", tOrFromTFunSingle $ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty Map.empty)) ])
        )
      ]
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
    [ TFunSingle ["F"] "I"
      (DeclaredPP $ PrePost preRecursive postRecursive)
    ]
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
    [ TFunSingle ["F", "I"] "I"
      (DeclaredPP $ PrePost preRecursive_withParams postRecursive_withParams)
    ]
    [ Return $ EFunCall "f" ["f", "i"]
    ]
  ]
bCall_recursive_withParams =
  bFun_recursive_withParams ++
  [ SetVar "i" cInt
  , Return $ EFunCall "f" ["f", "i"]
  ]
bIf_doubleObjectCreation =
  [ SetVar "f" $ EFunDef ["cond"] []
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
  [ SetVar "f" $ EFunDef ["cond"] []
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
  [ SetVar "f" $ EFunDef ["cond"] []
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
  [ SetVar "f" $ EFunDef ["cond"] []
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
  [ SetVar "f" $ EFunDef ["cond"] []
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
  [ SetVar "f" $ EFunDef ["cond", "x"] []
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
  [ SetVar "f" $ EFunDef ["cond", "y"] []
    [ If "cond"
      -- then
      [ SetVar "x" ENew ]
      -- else
      [ SetVar "x" ENew ]
    , Return $ EGetVar "x"
    ]
  ]
bGetUndefinedVar_inFunction =
  [ SetVar "f" $ EFunDef [] []
    [ Return $ EGetVar "x"
    ]
  ]
bIf_varUndefinedInThen_inFunction =
  [ SetVar "f" $ EFunDef ["cond", "y"] []
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
  [ SetVar "f" $ EFunDef ["x"] []
    [ SetVar "cond" (EBool True)
    , If "cond" [ ] [ SetVar "x" ENew ]
    , Return $ EGetVar "x"
    ]
  , SetVar "xx" ENew
  , Return $ EFunCall "f" ["xx"]
  ]
bIf_reassignInOneBranchWithNewCreatedOutsideOfIf_inFunction =
  [ SetVar "f" $ EFunDef ["x"] []
    [ SetVar "cond" (EBool True)
    , SetVar "y" ENew
    , If "cond" [ ] [ SetVar "x" $ EGetVar "y" ]
    , Return $ EGetVar "x"
    ]
  , SetVar "xx" ENew
  , Return $ EFunCall "f" ["xx"]
  ]
bIf_reassignInBothBranchesWithNew_inFunction =
  [ SetVar "f" $ EFunDef ["x"] []
    [ SetVar "cond" (EBool True)
    , If "cond" [ SetVar "x" ENew ] [ SetVar "x" ENew ]
    , Return $ EGetVar "x"
    ]
  , SetVar "xx" ENew
  , Return $ EFunCall "f" ["xx"]
  ]
bIf_reassignInOneBranchWithTheSameVar_inFunction =
  [ SetVar "f" $ EFunDef ["x"] []
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
bIfHasAttr_attributeUndefined_setAttrInElse =
  [ SetVar "x" ENew
  , IfHasAttr "x" "a" [ ] [ SetAttr "x" "b" cInt ]
  , Return $ EGetAttr "x" "b"
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
  -- x = new
  -- x.a = 42
  --
  -- // {x: X}, [X < {a: int}]
  -- // after weakening:
  -- // {x: X}, [X < {a v _|_: int}]
  --
  -- if x hasattr a:
  --   pass
  -- else:
  --   x.b = 7
  --
  -- // two possible typings:
  -- // 1) after (ifhttr+)
  -- //   {x: X}, [X < {a: int}]
  -- // 2) after (ifhttr)
  -- //   {x: X}, [X < {a: int v _|_, b: int v _|_}]

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
bFun_withSignature_intersectionExample_firstSignature =
  [ SetVar "f" $
      EFunDef ["x", "y"]
      -- func (X, Y) [X < {}, Y < {a: U}] -> U [X < {a: I}, Y < {a: U}, I < int]
      [ TFunSingle ["X", "Y"] "U"
        (DeclaredPP $ PrePost
          (Map.fromList [ ("X", tOrEmptyRec)
                        , ("Y", tOrFromTRec $ Map.fromList [ ("a", (Required "U")) ])
                        ])
          (Map.fromList [ ("X", tOrFromTRec $ Map.fromList [ ("a", (Required "I")) ])
                        , ("I", tOrPrimitive KInt)
                        , ("Y", tOrFromTRec $ Map.fromList [ ("a", (Required "U")) ])
                        ])
        )
      ]
      [ SetAttr "x" "a" cInt
      , Return $ EGetAttr "y" "a"
      ]
  ]
bCall_withSignature_intersectionExample_firstSignature =
  bFun_withSignature_intersectionExample_firstSignature ++
  [ SetVar "xx" ENew
  , SetVar "yy" ENew
  , SetAttr "yy" "a" (EBool True)
  , Return $ EFunCall "f" ["xx", "yy"]
  ]
bFun_withSignature_intersectionExample_secondSignature =
  [ SetVar "f" $
      EFunDef ["x", "y"]
      -- func (X, X) [X < {}] -> I [I < int, X < {a: I}]
      [ TFunSingle ["X", "X"] "I"
        (DeclaredPP $ PrePost
          (Map.fromList [ ("X", tOrEmptyRec)
                        ])
          (Map.fromList [ ("X", tOrFromTRec $ Map.fromList [ ("a", (Required "I")) ])
                        , ("I", tOrPrimitive KInt)
                        ])
        )
      ]
      [ SetAttr "x" "a" cInt
      , Return $ EGetAttr "y" "a"
      ]
  ]
bCall_withSignature_intersectionExample_secondSignature =
  bFun_withSignature_intersectionExample_secondSignature ++
  [ SetVar "xx" ENew
  , Return $ EFunCall "f" ["xx", "xx"]
  ]
bFun_withTwoSignatures_intersectionExample =
  [ SetVar "f" $
      EFunDef ["x", "y"]
      -- func (X, Y) [X < {}, Y < {a: U}] -> U [X < {a: I}, Y < {a: U}, I < int]
      [ TFunSingle ["X", "Y"] "U"
        (DeclaredPP $ PrePost
          (Map.fromList [ ("X", tOrEmptyRec)
                        , ("Y", tOrFromTRec $ Map.fromList [ ("a", (Required "U")) ])
                        ])
          (Map.fromList [ ("X", tOrFromTRec $ Map.fromList [ ("a", (Required "I")) ])
                        , ("I", tOrPrimitive KInt)
                        , ("Y", tOrFromTRec $ Map.fromList [ ("a", (Required "U")) ])
                        ])
        )
      , TFunSingle ["X", "X"] "I"
        (DeclaredPP $ PrePost
          (Map.fromList [ ("X", tOrEmptyRec)
                        ])
          (Map.fromList [ ("X", tOrFromTRec $ Map.fromList [ ("a", (Required "I")) ])
                        , ("I", tOrPrimitive KInt)
                        ])
        )
      ]
      [ SetAttr "x" "a" cInt
      , Return $ EGetAttr "y" "a"
      ]
  ]
bCall_withTwoSignatures_intersectionExample_firstSignature =
  bFun_withTwoSignatures_intersectionExample ++
  [ SetVar "xx" ENew
  , SetVar "yy" ENew
  , SetAttr "yy" "a" (EBool True)
  , Return $ EFunCall "f" ["xx", "yy"]
  ]
bCall_withTwoSignatures_intersectionExample_secondSignature =
  bFun_withTwoSignatures_intersectionExample ++
  [ SetVar "xx" ENew
  , Return $ EFunCall "f" ["xx", "xx"]
  ]
bCall_withTwoSignatures_intersectionExample_secondSignature_badArgs =
  bFun_withTwoSignatures_intersectionExample ++
  [ SetVar "xx" cInt
  , Return $ EFunCall "f" ["xx", "xx"]
  ]
bFun_withTwoSignatures_tooStrongPre =
  [ SetVar "f" $
      EFunDef ["x"]
      [ TFunSingle ["X"] "X" (DeclaredPP $ PrePost Map.empty Map.empty)
      , TFunSingle ["X"] "X" (DeclaredPP $ PrePost
                                        (Map.singleton "X" (tOrEmptyRec))
                                        (Map.singleton "X" (tOrEmptyRec)))
      ]
      [ Return $ EGetAttr "x" "a"
      ]
  ]
csEmptyRec       = (Map.singleton "X" tOrEmptyRec)
csRecWithOneAttr = (Map.fromList [ ("X", (tOrSingletonRec "a" "I"))
                                 , ("I", (tOrPrimitive KInt))
                                 ])
bFun_withTwoSignatures_tooWeakPost =
  [ SetVar "f" $
      EFunDef ["x"]
      [ TFunSingle ["X"] "X" (DeclaredPP $ PrePost csEmptyRec csEmptyRec)
      , TFunSingle ["X"] "X" (DeclaredPP $ PrePost csEmptyRec csRecWithOneAttr)
      ]
      [ SetAttr "x" "a" cInt
      , Return $ EGetVar "x"
      ]
  , SetVar "xx" ENew
  , Return $ EFunCall "f" ["xx"]
  ]

