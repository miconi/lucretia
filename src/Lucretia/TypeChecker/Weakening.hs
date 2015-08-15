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
  toWeaken <- weaker c c'
  (toWeaken == Map.empty) `orFail`
    ("Constraints: "++showConstraints c++" should be weaker or equal to "++showConstraints c')


weaker :: Constraints -- ^ @Constraints@ at the place of call
       -> Constraints -- ^ @Constraints@ at the place of declaration, function preconditions
       -> CM Constraints
weaker c c' = w c c'

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
  w :: a    -- ^ Post-Constraints at the place of call        (actual   conditions)
    -> a    -- ^  Pre-Constraints at the place of declaration (expected conditions)
    -> CM a -- ^ What should be added to preconstraints to ensure call' <c call' + decl, where call' = whatShouldBeAdded + m

instance Weaker Constraints where
  w c c' = do
    intersected <- sequence $ Map.intersectionWith w c c'
    let intersectedNonEmpty = Map.filter MapUtil.nonEmpty intersected
    let onlyInDecl = c' `Map.difference` c
    return $ intersectedNonEmpty `Map.union` onlyInDecl

instance Weaker TOr where
  w m m' = do
    (Set.isSubsetOf `on` Map.keysSet) m m' `orFail`
      ("Type: "++showTOr m++" should be weaker (have less possible types) then: "++showTOr m')
    rec <- (w `on` Map.lookup KRec) m m'
    return $ MapUtil.fromMaybe KRec rec

instance Weaker (Maybe TSingle) where
  w (Just (TRec s)) (Just (TRec s')) = do
    attributesToAdd <- MapUtil.combineWithM wTAttr s s'
    return $ if Map.null attributesToAdd
                then Nothing
                else Just $ TRec attributesToAdd
  w Nothing _ = return Nothing -- HERE Q: why Nothing as the left parameter?
  -- cannot be otherwise since:
  -- * types in "m :: TOr" are subset of types in "m' :: TOr": Set.isSubsetOf `on` Map.keys $ m m'
  -- * "w" on "Maybe TSingle" parameters are TRecs: rec <- (w `on` Map.lookup KRec) m m'

wTAttr :: Maybe TAttr -> Maybe TAttr -> CM (Maybe TAttr)
-- Nothing as a result means that the 'weaker' relation holds
-- (without a need to add any conditions).
-- wTAttr Nothing Nothing = Nothing -- commented out because Nothing from both maps is not possible
wTAttr Nothing t = return t
wTAttr (Just (Optional, i)) (Just (Required, i')) | i==i' = return $ Just (Required, i)
wTAttr (Just (Optional, _)) (Just (Required, _)) = throwError "Inside the programme a variable was referenced which may be undefined."
wTAttr _ _ = return Nothing

