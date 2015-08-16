-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Type update & merge rules.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances #-}

module Lucretia.TypeChecker.Update ( merge, update ) where

import Data.Function ( on )
import Control.Monad.Error ( throwError )

import Util.Map as MapUtil ( combineWithM, intersectionWithM, nonEmpty, unionWithM )

import Lucretia.Language.Definitions
import Lucretia.Language.Types
import Lucretia.TypeChecker.Monad


-- ** Type update rules (Fig 4. "The update operation." in wp)

class Update a where
  update :: a -> a -> CM a
instance Update Constraints where
  update = MapUtil.unionWithM update
instance Update TOr where
  update = MapUtil.combineWithM update
instance Update (Maybe TSingle) where
  update (Just (TRec r)) (Just (TRec r')) = return . Just . TRec =<< update r r'
  update _ t' = return t'
instance Update TRec where
  update = MapUtil.unionWithM update
instance Update TAttr where
  update  _               Forbidden                    = return Forbidden
  update  _            t@(WithPtr Required _ )         = return t
  update  Forbidden    t@(WithPtr Optional _ )         = return t
  update (WithPtr d i)   (WithPtr Optional i') | i==i' = return $ WithPtr d i
  -- IType pointers should be the same here.
  -- Renaming should throw an error when corresponding ITypes
  -- cannot be renamed to the same variable.


-- ** Type merging rules for the if-like instructions
-- (derived from weakening: Fig. 5. "Order over constraints." in wp)

-- | Merge Constraints from the 'then' and 'else' branches of the 'if' instruction.
merge :: PrePost -> PrePost -> CM PrePost
merge pp pp' = do
  preMerged  <- (mergePre  `on` _pre ) pp pp'
  postMerged <- (mergePost `on` _post) pp pp'
  return $ PrePost preMerged postMerged

-- | The only difference between update & mergePre is that:
-- update overrides a type in TOr, while
-- mergePre adds    a type in TOr
class MergePre a where
  mergePre :: a -> a -> CM a
instance MergePre Constraints where
  mergePre = MapUtil.unionWithM mergePre
instance MergePre TOr where
  mergePre t t' = do
    inBoth <- MapUtil.intersectionWithM mergePre t t'
    if MapUtil.nonEmpty inBoth
      then return inBoth
      else throwError $ "When merging preconditions from if branches: a conflicting type cannot be both "++show t++" and "++show t'++"."
instance MergePre TSingle where
  mergePre (TRec r) (TRec r') = return . TRec =<< mergePre r r'
  mergePre _ t = return t
instance MergePre TRec where
  mergePre = MapUtil.unionWithM mergePre
instance MergePre TAttr where
  -- IType pointers should be the same here.
  -- Renaming should throw an error when corresponding ITypes
  -- from 'then' & 'else' branches cannot be renamed to the same variable.
  mergePre t                      t'           | t == t' = return t
  mergePre t@(WithPtr Required _)    Forbidden           = cannotMerge
  mergePre    Forbidden           t@(WithPtr Required _) = cannotMerge
  mergePre   (WithPtr Optional _) t                      = return t
  mergePre t                        (WithPtr Optional _) = return t

cannotMerge = throwError $ "When merging preconditions from if branches: an attribute cannot be both required and forbidden."

-- | The only difference between mergePre & mergePost is that:
-- mergePre  picks the strongest Definedness for each TAttr in TRec, while
-- mergePost picks the weakest   Definedness for each TAttr in TRec
class MergePost a where
  mergePost :: a -> a -> CM a
instance MergePost Constraints where
  mergePost = MapUtil.unionWithM mergePost
instance MergePost TOr where
  mergePost = MapUtil.unionWithM mergePost
instance MergePost TSingle where
  mergePost (TRec r) (TRec r') = return . TRec =<< mergePost r r'
  mergePost _ t = return t -- in case of a function type: just select the second function type. The error will be already thrown when trying to mergePost two IType's in renaming
instance MergePost TRec where
  mergePost = MapUtil.combineWithM mergePost
instance MergePost (Maybe TAttr) where
  -- IType pointers should be the same here.
  -- Renaming should throw an error when corresponding ITypes
  -- from 'then' & 'else' branches cannot be renamed to the same variable.
  mergePost  t                    t'        | t == t' = return t
  mergePost (Just Forbidden)      Nothing             = return Nothing
  mergePost  Nothing             (Just Forbidden)     = return Nothing
  mergePost  _                   (Just (WithPtr _ i)) = return . Just $ WithPtr Optional i
  mergePost (Just (WithPtr _ i))  _                   = return . Just $ WithPtr Optional i

