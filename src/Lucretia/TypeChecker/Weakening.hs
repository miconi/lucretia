-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Weakening rules.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances #-}
module Lucretia.TypeChecker.Weakening ( checkWeaker, weaker ) where

import Data.Map ( Map )
import Data.Map as Map hiding ( update )
import Data.Set ( Set )
import Data.Set as Set hiding ( filter, update )
import Data.Function ( on )

import Control.Monad.Error ( throwError )
import Data.Traversable ( sequence )
import Prelude hiding ( sequence )

import Util.Map as MapUtil ( combineWithM, fromMaybe, nonEmpty )
import Util.OrFail ( orFail )

import Lucretia.Language.Definitions
import Lucretia.Language.Types
import Lucretia.TypeChecker.Monad


checkWeaker :: Constraints
            -> Constraints
            -> CM ()
checkWeaker c c' = do
  toWeaken <- weakerOnCs c c'
  (toWeaken == Map.empty) `orFail`
    ("Constraints: "++showConstraints c++" should be weaker or equal to "++showConstraints c')

type FreshPtrs = Set Ptr

weaker :: FreshPtrs   -- ^ fresh @Ptr@s created in the preceding block
       -> Constraints -- ^ @Constraints@ at the place of call
       -> Constraints -- ^ @Constraints@ at the place of declaration, function preconditions
       -> CM Constraints
weaker freshPtrs c c' = w (Just freshPtrs) c c'

weakerOnCs :: Constraints -- ^ @Constraints@ at the place of call
           -> Constraints -- ^ @Constraints@ at the place of declaration, function preconditions
           -> CM Constraints
weakerOnCs c c' = w Nothing c c'

class Weaker a where
  -- | Pump pre-constraints (looking at the post-constraints)
  -- of the whole code block (depicted as B) preceding function call,
  -- to match the called function's pre-constraints.
  -- 
  -- B ESth1
  -- B ESth2
  -- B ...
  -- B ESthN
  --   EFunCall
  w :: Maybe FreshPtrs
            -- ^ Pre-Constraints at the place of call, 'Just ptrs' for usage in
            -- bind function, 'Nothing' otherwise
    -> a    -- ^ Post-Constraints at the place of call        (actual   conditions)
    -> a    -- ^  Pre-Constraints at the place of declaration (expected conditions)
    -> CM a -- ^ What should be added to preconstraints to ensure call' <c call' + decl, where call' = whatShouldBeAdded + m

instance Weaker Constraints where
  w freshPtrs c c' = do
    intersected <- sequence $ Map.intersectionWithKey (wTOr freshPtrs) c c'
    let intersectedNonEmpty = Map.filter MapUtil.nonEmpty intersected
    let onlyInDecl = c' `Map.difference` c
    return $ intersectedNonEmpty `Map.union` onlyInDecl

--instance Weaker TOr where
wTOr :: Maybe FreshPtrs -> Ptr -> TOr -> TOr -> CM TOr
wTOr freshPtrs recPtr m m' = do
  (Set.isSubsetOf `on` Map.keysSet) m m' `orFail`
    ("Type: "++showTOr m++" should be weaker (have less possible types) then: "++showTOr m')
  rec <- (wTSingle (fmap (Set.member recPtr) freshPtrs) `on` Map.lookup KRec) m m'
  return $ MapUtil.fromMaybe KRec rec

--instance Weaker (Maybe TSingle) where
wTSingle :: Maybe Bool -> Maybe TSingle -> Maybe TSingle -> CM (Maybe TSingle)
wTSingle isRecFresh (Just (TRec s)) (Just (TRec s')) = do
  attributesToAdd <- MapUtil.combineWithM (wTAttr isRecFresh) s s'
  return $ if Map.null attributesToAdd
              then Nothing
              else Just $ TRec attributesToAdd
wTSingle _ Nothing _ = return Nothing
-- cannot be otherwise since:
-- * types in "m :: TOr" are subset of types in "m' :: TOr": Set.isSubsetOf `on` Map.keys $ m m'
-- * "wTSingle" parameters are TRecs: rec <- (wTSingle isRecFresh `on` Map.lookup KRec) m m'

--instance Weaker (Maybe TAttr) where
wTAttr :: Maybe Bool -> Maybe TAttr -> Maybe TAttr -> CM (Maybe TAttr)
-- Nothing as a result means that the 'weaker' relation holds
-- (without a need to add any conditions).

-- This special case is implementing the (bot) rule, i.e. the bottom introduction rule
wTAttr (Just True) Nothing (Just Forbidden) = return Nothing

wTAttr _ Nothing t = return t
wTAttr _ (Just (Optional i)) t@(Just (Required i')) | i==i' = return t
wTAttr _ (Just (Optional _))   (Just (Required _ )) = throwError "Attribute is required but it may be undefined."
wTAttr _ (Just (Optional _))   (Just  Forbidden   ) = throwError "Attribute is forbidden but it may have been defined."
wTAttr _ (Just (Required _))   (Just  Forbidden   ) = throwError "Attribute is forbidden but it was defined."
wTAttr _ (Just  Forbidden  )   (Just (Required _ )) = throwError "Attribute is required but it was not defined."
wTAttr _ _ _ = return Nothing

