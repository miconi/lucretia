-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Types used in Lucretia TypeChecker and in function signature declarations.
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts #-}

module Lucretia.Language.Types where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Util.OrFail ( orFail )

import Lucretia.Language.Definitions

import Data.Lens (Lens, mapLens, getL)
import Data.Lens.Template (makeLens)
import Prelude hiding ((.))
import Control.Category ((.))
import Util.MapLenses (mapInsertLens)


-- * Show instance

showProgrammeType :: ProgrammeType -> String
showProgrammeType (Left msg) = "Error: "++msg
showProgrammeType (Right (i, cs)) = i++" with Constraints: "++showConstraints cs

showType :: Type -> String
showType (i, pp) = i++", "++showPrePost pp

showPrePost :: PrePost -> String
showPrePost (PrePost pre post) = concat
  [ "Pre="
  , showConstraints pre
  , " Post="
  , showConstraints post
  ]

showConstraints :: Constraints -> String
showConstraints cs = concat ["[", showFields cs, "]"]
  where
  showFields cs = intercalate ", " (map showField $ Map.toList cs)
  showField (l, t) = concat [l," < ", showTOr t]

showTOr :: TOr -> String
showTOr tOr = intercalate " v " $ map showKind $ Map.toList tOr

showKind :: (Kind, TSingle) -> String
showKind (KInt,    _) = "int"
showKind (KString, _) = "string"
showKind (KBool,   _) = "bool"
showKind (KNone,   _) = "None"
showKind (KRec, TRec t) = showRec t
showKind (KFun, TFun f) = showFun f
showKind other          = show other

showFun :: TFunSingle -> String
showFun (TFunSingle argIds returnId funPP) = concat
  [ "func ("
  , intercalate ", " argIds
  , ") "
  , showFunPP _pre  funPP
  , " -> "
  , returnId
  , " "
  , showFunPP _post funPP
  ]

showFunPP :: (PrePost -> Constraints) -> FunPrePost -> String
showFunPP _ InheritedPP = ""
showFunPP which (DeclaredPP pp) = showConstraints $ which pp

showRec :: TRec -> String
showRec r = concat ["{", showFields r, "}"]
  where
  showFields r = intercalate ", " $ map showField $ Map.toList r
  showField (a,  Forbidden          ) = concat ["forbidden ", a]
  showField (a, (WithPtr Required i)) = concat [a, ": ", i]
  showField (a, (WithPtr Optional i)) = concat ["optional ", a, ": ", i]


-- * Language.Types (/Defition 2.1 (Language.Types)/ in wp)
-- in one sentence: IAttr ~> TAttr = (Definedness, Ptr) ~> TOr = Map Kind TSingle

type ProgrammeType = Either ErrorMsg (Ptr, Constraints)

type Type = (Ptr, PrePost)

-- | A mapping from type-variable names to types.
-- List of pairs @X <# t_r@ in wp.
type Constraints = Map Ptr TOr

-- Invariant: TOr can contain at most one TInt, one TString, TRec, ...
-- This is achieved by having a Map with Kind as the type of keys.
-- Invariant: TOr contains at least one Kind / single type.
type TOr         = Map Kind TSingle

data Kind        = KInt
                 | KString
                 | KBool
                 | KNone
                 | KRec
                 | KFun

                 deriving ( Eq, Ord, Show )

data TSingle     = TRec TRec
                 -- | TFunOr TFunOr --TODO intersection types
                 | TFun TFunSingle
                 | TPrimitive -- kind without parameters

                 deriving ( Eq, Ord, Show )

-- | Record (models an object). A mapping from attr names to Language.Types.
-- List of pairs @l : t@ in wp.
-- It is the same type as 'Env'
type TRec = Map IAttr TAttr

data TAttr = Forbidden | WithPtr Definedness Ptr
  deriving (Eq, Ord, Show)

data Definedness = Required | Optional
  deriving (Eq, Ord, Show)

type TFun = Maybe TFunSingle
--data TFun = Maybe TFunOr
--type TFunOr        = Set TFunSingle
data TFunSingle  = TFunSingle { funArgs :: [Ptr]
                              , funRet  :: Ptr
                              , funPP   :: FunPrePost
                              }
                 deriving ( Eq, Ord, Show )

data FunPrePost  = InheritedPP | DeclaredPP PrePost
                 deriving ( Eq, Ord, Show )

-- * Constraints (@Psi@ in wp)

-- | Pre- and post-Constraints (respectively: the left and the right
-- hand side of a type-checker rule / function signature).
data PrePost = PrePost { _pre  :: Constraints
                       , _post :: Constraints
                       }
                 deriving ( Eq, Ord )

instance Show PrePost where
  show = showPrePost

-- TODO RTR with Conditions in place of Constraints:
-- data Conditions = Conditions { _environment :: TRec
--                              , _constraints :: Constraints
--                              }
-- + no special handling in renaming
-- + no special entry in emptyConstraints
-- + no assertions in getEnv, fromEnv

$(makeLens ''PrePost)

emptyPrePost :: PrePost
emptyPrePost = PrePost emptyConstraints emptyConstraints

emptyConstraints :: Constraints
emptyConstraints = Map.singleton env $ tOrEmptyRec

-- showConstraints c = concat ["[",showAttrs attrs,"]"] where
--   attrs = Map.toList c
--   showAttrs attrs = intercalate ", " (map showAttr attrs)
--   showAttr (l,t) = concat [l," < ", showRec t]

-- * Record Type (@t_r = {l : t} | {}@ in wp)

-- showRec :: TRec -> String 
-- showRec r = concat ["{",showAttrs attrs,"}"] where
--   attrs = Map.toList r 
--   showAttrs attrs = intercalate ", " (map showAttr attrs)
--   showAttr (l,t) = concat [l,":",show t]

emptyRec :: TRec
emptyRec = Map.empty

env :: Ptr
env = "Env"

xId :: Ptr
xId = "xId"

aId :: Ptr
aId = "aId"

yId :: Ptr
yId = "yId"

fId :: Ptr
fId = "fId"

undefinedId :: Ptr
undefinedId = "undefinedId"

toEmptyRec id = (id, tOrEmptyRec)

tOrEmptyRec :: TOr
tOrEmptyRec = tOrFromTRec emptyRec

-- envToX :: IAttr -> (Ptr, TOr)
-- envToX x = toSingletonRec env x xId

toSingletonRec :: Ptr -> IAttr -> Ptr -> (Ptr, TOr)
toSingletonRec xId a aId = (xId, tOrSingletonRec a aId)

tOrSingletonRec :: IAttr -> Ptr -> TOr
tOrSingletonRec a t = tOrFromTRec $ Map.singleton a (WithPtr Required t)

tOrFromTSingle :: TSingle -> TOr
tOrFromTSingle tSingle = Map.singleton (kind tSingle) tSingle

tOrFromTRec :: TRec -> TOr
tOrFromTRec = tOrFromTSingle . TRec

tOrFromTFun :: TFunSingle -> TOr
tOrFromTFun = tOrFromTSingle . TFun

tOrFromTFunSingle :: TFunSingle -> TOr
tOrFromTFunSingle = tOrFromTSingle . TFun

-- | "env" type pointer is always present in Constraints and it is always a record
getEnv :: Constraints -> TRec
getEnv cs = tRec
  where TRec tRec = tOr Map.! KRec
        tOr = Map.findWithDefault tOrEmptyRec env cs

lookupInEnv :: IVar -> Constraints -> Maybe TAttr
lookupInEnv x cs = Map.lookup x $ getEnv cs

-- | Lookup 'TOr' for a given 'Ptr'.
--
-- The function will return the corresponding values as @('Just' value)@,
-- or 'Nothing' if 'Ptr' refers to a polymorphic type.
lookupInConstraints :: Ptr -> Constraints -> Maybe TOr
lookupInConstraints = Map.lookup

singletonConstraint :: Ptr -> TOr -> Constraints
singletonConstraint = Map.singleton

singletonTRec :: IAttr -> TAttr -> TRec
singletonTRec = Map.singleton

tOrPrimitive :: Kind -> TOr
tOrPrimitive kind = Map.singleton kind TPrimitive

kind :: TSingle -> Kind
kind (TRec _) = KRec
kind (TFun _) = KFun

