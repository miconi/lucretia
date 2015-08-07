-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Map utility functions.
-----------------------------------------------------------------------------
module Util.Map ( lookupAll, findAll, mapCombineWith, mapFromMaybe, nonEmpty ) where

import Data.Map ( Map )
import Data.Map as Map hiding ( filter, update )
import Data.Set as Set hiding ( filter, update )
import Data.Function ( on )


-- | Lookup values for given keys.
--
-- The function will return the corresponding values as @('Just' value)@,
-- or 'Nothing' if a key isn't in the map.
lookupAll :: Ord k
          => [k] -> Map k v
          -> [Maybe v]
lookupAll keys m = fmap (\k -> Map.lookup k m) keys

-- | Find values for given keys.
-- Calls 'error' when any of the elements can not be found.
findAll   :: Ord k
          => [k] -> Map k v
          -> [v]
findAll   keys m = fmap (m Map.!) keys

-- | Merge two Maps using a combiner function.
mapCombineWith :: Ord k
               => (Maybe v -> Maybe v -> Maybe v')
               -> Map k v -> Map k v -> Map k v'
mapCombineWith combine m m' =
  Map.fromList $
    filterJust $
      fmap (applyCombine combine m m') $
        Set.toList $ allKeys m m'
  where allKeys :: Ord k => Map k v -> Map k v -> Set k
        allKeys = Set.union `on` keysSet
        applyCombine :: Ord k
                     => (Maybe v -> Maybe v -> Maybe v')
                     -> Map k v -> Map k v
                     -> k
                     -> (k, Maybe v')
        applyCombine combine m m' k =
          ( k
          , (combine `on` Map.lookup k) m m'
          )
        filterJust :: [(k, Maybe v)] -> [(k, v)]
        filterJust xs = fmap (\(k, Just v) -> (k, v)) $ filter isJust xs
        isJust :: (k, Maybe v) -> Bool
        isJust (_, Just _ ) = True
        isJust (_, Nothing)  = False

-- | Create a Map from a Maybe value.
--
-- Nothing results in an empty Map, Just value creates a Map with a single
-- entry with the value associated with a key given as the first parameter
-- of this function.
mapFromMaybe :: Ord k => k -> Maybe v -> Map k v
mapFromMaybe k v =
    Map.alter (\_ -> v) k Map.empty

-- | Is the map not empty?
nonEmpty :: Map k v -> Bool
nonEmpty = not . Map.null

