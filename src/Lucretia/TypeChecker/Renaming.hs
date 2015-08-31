-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Renaming on Record Identifiers ('Ptr' @->@ 'Ptr').
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

--module Lucretia.TypeChecker.Renaming where
module Lucretia.TypeChecker.Renaming ( applyRenaming, freeVariables, getRenamingOnEnv, getRenaming, getRenamingPP, Renaming, RenamingType(..) ) where

import Prelude hiding ( any, sequence )

import Data.Map ( Map )
import Data.Map as Map
import Data.Set ( Set )
import Data.Set as Set
import Data.Foldable ( any, Foldable )
import Data.Function ( on )
import Data.Tuple ( swap )

import Control.Monad.Error ( runErrorT, throwError, ErrorT )
import Control.Monad.Identity ( runIdentity, Identity )
import Control.Monad.State ( execStateT, get, lift, modify, StateT )
import Control.Monad.Trans.Reader ( ask, asks, ReaderT, runReaderT )

import Util.Map ( findAll )
import Util.OrFail ( orFail )

import Lucretia.Language.Definitions
import Lucretia.Language.Syntax
import Lucretia.Language.Types
import Lucretia.TypeChecker.Monad ( CM )


-- | Get all free occuring variables.
class FreeVariables a where
  freeVariables :: a -> Set Ptr
instance FreeVariables Renaming where
  freeVariables r = Set.map fst r `Set.union` Set.map snd r
instance FreeVariables PrePost where
  freeVariables (PrePost pre post) = (Set.union `on` freeVariables) pre post
instance FreeVariables Constraints where
  freeVariables cs = Set.delete env (Map.keysSet cs) `Set.union` freeVariables (Map.elems cs)
instance FreeVariables a => FreeVariables [a] where
  freeVariables = Set.unions . fmap freeVariables
instance FreeVariables TOr where
  freeVariables = freeVariables . Map.elems
instance FreeVariables TSingle where
  freeVariables (TRec rec) = (freeVariables . Map.elems) rec
  freeVariables (TFun ts) = Set.unions $ fmap freeVariables ts
  freeVariables _ = Set.empty
instance FreeVariables TAttr where
  freeVariables (Required i) = Set.singleton i
  freeVariables (Optional i) = Set.singleton i
  freeVariables  Forbidden   = Set.empty
instance FreeVariables TFunSingle where
  freeVariables (TFunSingle argTypes iDecl funPP) =
    Set.fromList argTypes `Set.union`
    Set.singleton iDecl `Set.union`
    freeVariables funPP
instance FreeVariables FunPrePost where
  freeVariables (DeclaredPP pp) = freeVariables pp
  freeVariables InheritedPP = Set.empty

-- | Renaming to actual 'Ptr' (at call) from expected 'Ptr' (at declaration).
--
-- Also serves as a list of visited node pairs.
type Renaming = Set (Ptr, Ptr)

emptyRenaming :: Renaming
emptyRenaming = Set.empty

-- | Rename using renaming
applyRenaming :: ApplyRenaming t
              => Renaming
              -> t -> t
applyRenaming renaming = ar $ functionFromSetOfPairs $ Set.map swap renaming
  where
  functionFromSetOfPairs :: Set (Ptr, Ptr) -> Ptr -> Ptr
  functionFromSetOfPairs set = fromMap $ Map.fromList . Set.toList $ set

  fromMap :: Map.Map Ptr Ptr -> Ptr -> Ptr
  fromMap map i = Map.findWithDefault i i map

class ApplyRenaming t where
  -- | Rename the whole nested structure, starting from Ptr in Constraints
  -- and ending on Ptr in TOr. Do not rename recursively, there is no need to do it.
  ar :: (Ptr -> Ptr) -> t -> t
instance ApplyRenaming Type where
  ar f (i, pp) = (f i, ar f pp)
instance ApplyRenaming Ptr where
  ar f = f
instance ApplyRenaming FunPrePost where
  ar f (DeclaredPP pp) = DeclaredPP $ ar f pp
  ar f InheritedPP = InheritedPP
instance ApplyRenaming PrePost where
  ar f (PrePost pre post) =
    PrePost
      (ar f pre)
      (ar f post)
instance ApplyRenaming Constraints where
  ar f = Map.mapKeys f . Map.map (ar f)
instance ApplyRenaming TOr where
  ar f = Map.map (ar f)
instance ApplyRenaming TSingle where
  ar f (TRec rec) = TRec $ Map.map (ar f) rec
  ar f (TFun fun) = TFun $ ar f fun
  ar _ t = t
instance ApplyRenaming TAttr where
  ar f (Required i) = Required (f i)
  ar f (Optional i) = Optional (f i)
  ar f  Forbidden   = Forbidden
instance ApplyRenaming TFun where
  ar f = fmap (ar f)
instance ApplyRenaming TFunSingle where
  ar f (TFunSingle preTs postT ppF) =
    TFunSingle 
      (f `fmap` preTs)
      (f postT)
      (ar f ppF)
 
data RenamingType = FullRenaming | BindRenaming (Set Ptr)

getRenamingOnEnv :: RenamingType -> Constraints -> Constraints -> CM Renaming
getRenamingOnEnv rt = getRenamingOnEnvWith rt emptyRenaming

getRenamingOnEnvWith :: RenamingType -> Renaming -> Constraints -> Constraints -> CM Renaming
getRenamingOnEnvWith rt renaming = getRenamingWith rt renaming `on` \cs -> (getEnv cs, cs)

getRenamingPP :: RenamingType -> PrePost -> PrePost -> CM Renaming
getRenamingPP rt (PrePost pre post) (PrePost pre' post') = do
  renaming <- getRenamingOnEnv rt pre pre'
  getRenamingOnEnvWith rt renaming post post'

class GetRenaming a where
  -- | Gets renaming.
  -- Resulting Renaming does not contain identity pairs.
  -- Expected condition should be w or equal to actual condition.
  getRenaming :: RenamingType
              -> (a, Constraints) -- ^ @a@ at the place of call
              -> (a, Constraints) -- ^ @a@ at the place of declaration
              -> CM Renaming       -- ^ Possible renamings
  getRenaming rt = getRenamingWith rt emptyRenaming

  getRenamingWith :: RenamingType
                  -> Renaming
                  -> (a, Constraints)
                  -> (a, Constraints)
                  -> CM Renaming
  getRenamingWith rt renaming (a, cs) (a', cs') = fmap removeIdentities $ lift $ execStateT (runReaderT (r a a') (Parameters rt (cs, cs'))) renaming
    where
    -- The Renaming property of having no identity pairs is used in the implementation of the if rule.
    removeIdentities :: Renaming -> Renaming
    removeIdentities = Set.filter (\(x, y) -> not $ x == y)

  r :: a -- ^ Get renaming to this …
    -> a -- ^ … from that.
    -> M ()
type M = ReaderT Parameters (StateT Renaming (ErrorT ErrorMsg Identity))
type Environment = (Constraints, Constraints)
data Parameters = Parameters { renamingType :: RenamingType
                             , environment :: Environment
                             }

ok = return ()
getVisited :: M Renaming
getVisited = get
asksConstraints :: (Environment -> Constraints) -> M Constraints
asksConstraints f = asks (f . environment)

instance GetRenaming [Ptr] where
  r (i:is) (i':is') = r i i' >> r is is'
  r []     []       = ok
instance GetRenaming Ptr where
  r i i' = do
    visited <- getVisited
    if alreadyFollowed (i, i') visited
      then ok
      else checkRecursively i i'
    where
      alreadyFollowed = Set.member

      checkRecursively i i' = do
        visited <- getVisited
        ((i, i') `neitherMemberOf` visited)
        modify $ Set.insert (i, i')
        t  <- ptrFromTAttrFromConstraints i  fst
        t' <- ptrFromTAttrFromConstraints i' snd
        r t t'

      ptrFromTAttrFromConstraints i which = do
        cs <- asksConstraints which
        return $ Map.lookup i cs

      neitherMemberOf :: (Ptr, Ptr) -> Renaming -> M ()
      (i, i') `neitherMemberOf` visited = do
        cs <- asks environment
        let errorDetails = ". Error occured while tried to get renaming from: "++showConstraints (snd cs)++ " to: "++showConstraints (fst cs)
        not (memberFst i  visited) `orFail` ("There are multiple variables that should be renamed to "++i++errorDetails)
        not (memberSnd i' visited) `orFail` ("There are multiple variables that should be renamed from "++i'++errorDetails)
        where
        memberFst :: (Eq a, Foldable f) => a -> f (a, b) -> Bool
        memberFst x = any $ \(y, _) -> x == y
        memberSnd :: (Eq b, Foldable f) => b -> f (a, b) -> Bool
        memberSnd x = any $ \(_, y) -> x == y
instance GetRenaming (Maybe TOr) where
  r (Just t) (Just t') = r t t'
  r _ _ = ok
  -- get renaming only where possible
  -- i.e. where there is a 'TOr' defined in 'Constraints' for a corresponding 'Ptr'
instance GetRenaming TOr where
  r = r `on` Map.lookup KRec
instance GetRenaming (Maybe TSingle) where
  r (Just (TRec t)) (Just (TRec t')) = r t t'
  r _  _ = ok
  -- get renaming only where possible
  -- i.e. where there is are 'KRec' defined in both 'TOr' values
instance GetRenaming TRec where
  r t t' = (r `on` inBoth) t t'
    where
      inBoth = findAll (Set.toList keysInBoth)
      keysInBoth :: Set IAttr
      keysInBoth = Map.keysSet t `Set.intersection` Map.keysSet t'
instance GetRenaming [TAttr] where
  r (i:is) (i':is') = r i i' >> r is is'
  r []     []       = ok
instance GetRenaming TAttr where
  r Forbidden _ = ok
  r _ Forbidden = ok
  r a a' = do
    rt <- asks renamingType
    case rt of
      FullRenaming                -> rFull a a'
      BindRenaming freshVariables -> rBind a a' freshVariables

    where
      rFull a a' = r (ptrFromTAttr a) (ptrFromTAttr a')

      rBind (Optional i) (Required i') freshVariables =
        if i `Set.member` freshVariables
          then throwError $ "Possibly undefined variable was referenced. Cannot merge a fresh type pointer (i.e. created inside an if instruction) with a stale type pointer (i.e. one that should be created before the if instruction, to make sure that the referenced variable is defined)."
          else r i i'
      rBind a a' _ = rFull a a'

