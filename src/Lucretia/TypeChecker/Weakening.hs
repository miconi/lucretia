-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Weakening rules.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances #-}
module Lucretia.TypeChecker.Weakening ( funDefWeaker, funAppWeaker ) where

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

type FreshPtrs = Set Ptr

data WeakeningType = FunDefWeakening | FunAppWeakening

funDefWeaker :: FreshPtrs   -- ^ fresh @Ptr@s created in the preceding block
             -> Constraints -- ^ Post@Constraints@ infered from the function body
             -> Constraints -- ^ Post@Constraints@ declared in the function signature
             -> CM ()
funDefWeaker freshPtrs c c' = do
  w FunDefWeakening freshPtrs c c'
  return ()

-- | Pump pre-constraints (looking at the post-constraints)
-- of the whole code block (depicted as B) preceding function call,
-- to match the called function's pre-constraints.
--
-- B ESth1
-- B ESth2
-- B ...
-- B ESthN
--   EFunCall
funAppWeaker :: FreshPtrs      -- ^ fresh @Ptr@s created in the preceding block
             -> Constraints    -- ^ Post@Constraints@ at the place of call        (actual   conditions)
             -> Constraints    -- ^  Pre@Constraints@ at the place of declaration (expected conditions)
             -> CM Constraints -- ^ What should be added to preconstraints to ensure:
                               -- call' <c call' + decl, where call' = whatShouldBeAdded + m
funAppWeaker = w FunAppWeakening

class Weaker a where
  w :: WeakeningType
    -> FreshPtrs -- ^ fresh @Ptr@s created in the preceding block
    -> a
    -> a
    -> CM a

instance Weaker Constraints where
  w FunDefWeakening freshPtrs c c' = do
    ((==) `on` Map.keys) c c' `orFail`
      ("The list of type pointer names should be the same in actual and declared function postconstraints: "++showConstraints c++" and "++showConstraints c')
    intersected <- sequence $ Map.intersectionWithKey (wTOr FunDefWeakening freshPtrs) c c'
    return Map.empty
  w FunAppWeakening freshPtrs c c' = do
    intersected <- sequence $ Map.intersectionWithKey (wTOr FunAppWeakening freshPtrs) c c'
    let intersectedNonEmpty = Map.filter MapUtil.nonEmpty intersected
    let onlyInDecl = c' `Map.difference` c
    return $ intersectedNonEmpty `Map.union` onlyInDecl

-- Almost instance Weaker TOr where
wTOr :: WeakeningType -> FreshPtrs -> Ptr -> TOr -> TOr -> CM TOr
wTOr wt freshPtrs recPtr m m' = do
  (Set.isSubsetOf `on` Map.keysSet) m m' `orFail`
    ("Type: "++showTOr m++" should be weaker (have less possible types) then: "++showTOr m')
  rec <- (wTSingle wt (recPtr `Set.member` freshPtrs) `on` Map.lookup KRec) m m'
  return $ MapUtil.fromMaybe KRec rec

-- Almost instance Weaker (Maybe TSingle) where
wTSingle :: WeakeningType -> Bool -> Maybe TSingle -> Maybe TSingle -> CM (Maybe TSingle)
wTSingle FunDefWeakening isRecFresh (Just (TRec s)) (Just (TRec s')) = do
  ((==) `on` Map.keys) s s' `orFail` 
    ("The list of attribute names should be the same in actual and declared records in function postconstraints: "++showRec s++" and "++showRec s')
  attributesToAdd <- MapUtil.combineWithM (nothingToForbidden isRecFresh) s s'
  if Map.null attributesToAdd
    then return Nothing
    else throwError "Attribute is required but it may be undefined."
wTSingle FunAppWeakening isRecFresh (Just (TRec s)) (Just (TRec s')) = do
  attributesToAdd <- MapUtil.combineWithM (nothingToForbidden isRecFresh) s s'
  return $ if Map.null attributesToAdd
              then Nothing
              else Just $ TRec attributesToAdd

wTSingle _ _ Nothing _ = return Nothing
-- cannot be otherwise since:
-- * types in "m :: TOr" are subset of types in "m' :: TOr": Set.isSubsetOf `on` Map.keys $ m m'
-- * "wTSingle" parameters are TRecs: rec <- (wTSingle isRecFresh `on` Map.lookup KRec) m m'

-- Implementation of the (bot) rule, i.e. the bottom introduction rule
-- Almost instance Weaker (Maybe TAttr) where
nothingToForbidden :: Bool -> Maybe TAttr -> Maybe TAttr -> CM (Maybe TAttr)
nothingToForbidden True Nothing t' = wTAttr (Just Forbidden) t'
nothingToForbidden _    t       t' = wTAttr  t               t'

-- Nothing as a result means that the 'weaker' relation holds
-- (without a need to add any conditions).
-- Almost instance Weaker (Maybe TAttr) where
wTAttr :: Maybe TAttr -> Maybe TAttr -> CM (Maybe TAttr)
wTAttr Nothing t = return t
wTAttr (Just (Optional i)) t@(Just (Required i')) | i==i' = return t
wTAttr (Just (Optional _))   (Just (Required _ )) = throwError "Cannot merge different type pointers."
wTAttr (Just (Optional _))   (Just  Forbidden   ) = throwError "Attribute is forbidden but it may have been defined."
wTAttr (Just (Required _))   (Just  Forbidden   ) = throwError "Attribute is forbidden but it was defined."
wTAttr (Just  Forbidden  )   (Just (Required _ )) = throwError "Attribute is required but it was not defined."
wTAttr _ _ = return Nothing


