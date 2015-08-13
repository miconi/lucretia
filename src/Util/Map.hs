-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Map utility functions.
-----------------------------------------------------------------------------
module Util.Map ( combineWith, combineWithM, findAll, fromMaybe, lookupAll, nonEmpty, unionWithM ) where

import Prelude hiding ( sequence )
import Data.Traversable ( sequence )

import Data.Map ( Map )
import Data.Map as Map hiding ( filter, update )
import Data.Set as Set hiding ( filter, update )
import Data.Function ( on )


-- | Merge two Maps using a combiner function.
combineWith :: Ord k
            => (Maybe v -> Maybe v -> Maybe v')
            -> Map k v -> Map k v -> Map k v'
combineWith combine mapA mapB =
  Map.fromList $
    filterJust $
      fmap (applyCombine combine mapA mapB) $
        Set.toList $ allKeys mapA mapB
  where applyCombine :: Ord k
                     => (Maybe v -> Maybe v -> Maybe v')
                     -> Map k v -> Map k v
                     -> k
                     -> (k, Maybe v')
        applyCombine combine mapA mapB k =
          let v = (combine `on` Map.lookup k) mapA mapB
          in (k, v)

allKeys :: Ord k => Map k v -> Map k v -> Set k
allKeys = Set.union `on` keysSet

filterJust :: [(k, Maybe v)] -> [(k, v)]
filterJust xs = fmap (\(k, Just v) -> (k, v)) $ filter isJust xs

isJust :: (k, Maybe v) -> Bool
isJust (_, Just _ ) = True
isJust (_, Nothing)  = False

-- | Monadic version of combineWith.
combineWithM :: (Monad m, Ord k)
             => (Maybe v -> Maybe v -> m (Maybe v'))
             -> Map k v -> Map k v -> m (Map k v')
combineWithM combine mapA mapB =
  -- Conversion goes like this: [(k, m v)] ~> [m (k, v)] ~> m [(k, v)]
  return . Map.fromList . filterJust =<<
    (sequence $ fmap (applyCombine combine mapA mapB) $ Set.toList $ allKeys mapA mapB)
  where applyCombine :: (Monad m, Ord k)
                     => (Maybe v -> Maybe v -> m (Maybe v'))
                     -> Map k v -> Map k v
                     -> k
                     -> m (k, Maybe v')
        applyCombine combine mapA mapB k = do
          v <- (combine `on` Map.lookup k) mapA mapB
          return (k, v)

-- | Find values for given keys.
-- Calls 'error' when any of the elements can not be found.
findAll   :: Ord k
          => [k] -> Map k v
          -> [v]
findAll   keys m = fmap (m Map.!) keys

-- | Create a Map from a Maybe value.
--
-- Nothing results in an empty Map, Just value creates a Map with a single
-- entry with the value associated with a key given as the first parameter
-- of this function.
fromMaybe :: Ord k => k -> Maybe v -> Map k v
fromMaybe k v = Map.alter (\_ -> v) k Map.empty

-- | Lookup values for given keys.
--
-- The function will return the corresponding values as @('Just' value)@,
-- or 'Nothing' if a key isn't in the map.
lookupAll :: Ord k
          => [k] -> Map k v
          -> [Maybe v]
lookupAll keys m = fmap (\k -> Map.lookup k m) keys

-- | Is the map not empty?
nonEmpty :: Map k v -> Bool
nonEmpty = not . Map.null

-- | Monadic version of Map.unionWith.
unionWithM :: (Monad m, Ord k) => (v -> v -> m v) -> Map k v -> Map k v -> m (Map k v)
unionWithM f mapA mapB =
  sequence $ unionWith (\mx my -> do {x <- mx; y <- my; f x y}) (fmap return mapA) (fmap return mapB)

