-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Update- & Extend- Constraints rules.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Lucretia.TypeChecker.Update ( merge, update, extend ) where

import Data.Map ( Map )
import Data.Map as Map hiding ( filter, update )
import Data.Set as Set hiding ( filter, update )
import Data.Function ( on )

import Util.Map ( mapCombineWith )

import Lucretia.Language.Definitions
import Lucretia.Language.Types


-- ** Type information update (Definition 3.4 in wp)
class Update a where
  update :: a -> a -> a
instance Update Constraints where
  update = Map.unionWith update
instance Update TOr where
  update = mapCombineWith update
instance Update (Maybe TSingle) where
  update (Just (TRec r)) (Just (TRec r')) = Just $ TRec (update r r')
  -- TODO update (Just (TRec r)) Nothing = Just $ TRec r
  update _ t' = t'
instance Update TRec where
  update = Map.unionWith update
instance Update TAttr where
  update _                t'@(Required, _)  = t'
  update (definedness, i)    (Optional, i') = (definedness, merge i i')
    where merge i i' = if i == i' then i else undefinedId
          -- HERE TODO should throw an error: cannot merge when overriding a type pointer with an different optional type pointer
  -- IType must match in both sides

-- | The only difference between update & extend is that:
-- update overrides a type in TOr, while
-- extend adds      a type in TOr
class Extend a where
  extend :: a -> a -> a
instance Extend Constraints where
  extend = Map.unionWith extend
instance Extend TOr where
  extend = Map.unionWith extend
instance Extend TSingle where
  extend (TRec r) (TRec r') = TRec $ update r r'
  extend _ t = t

-- | The only difference between extend & merge is that:
-- extend adds       attributes in TRec, while
-- merge  intersects attributes in TRec
class Merge a where
  merge :: a -> a -> a
instance Merge PrePost where
  merge pp pp' =
    PrePost ((merge `on` _pre ) pp pp')
            ((merge `on` _post) pp pp')
instance Merge Constraints where
  merge = Map.unionWith merge
instance Merge TOr where
  merge = Map.unionWith merge
instance Merge TSingle where
  merge (TRec r) (TRec r') = TRec $ merge r r'
  merge _ t = t -- in case of a function type: just select the second function type. The error will be already thrown when trying to merge two IType's in renaming
instance Merge TRec where
  merge = mapCombineWith merge
instance Merge (Maybe TAttr) where
  -- IType pointers should be the same here.
  -- Renaming should throw an error when corresponding ITypes
  -- from 'then' & 'else' branches cannot be renamed to the same variable.
  -- But the error is found earlier, by checking that the renaming for variables
  -- not in the set of fresh variables created in the if branches.
  merge (Just (Required, i)) (Just (Required, i')) | i==i' = Just (Required, i)
  merge (Just (_       , i)) (Just (_       , i')) | i==i' = Just (Optional, i)
  merge  Nothing             (Just (_       , i))          = Just (Optional, i)
  merge (Just (_       , i))  Nothing                      = Just (Optional, i)

