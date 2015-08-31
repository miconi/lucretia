-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- TypeChecker monad. The monad is serving two purposes:
-- * backtracking
-- * serving fresh 'Ptr' variable names.
-----------------------------------------------------------------------------
module Lucretia.TypeChecker.Monad where

import Prelude hiding ( any, sequence )

import Data.Traversable ( sequence, Traversable )

import Control.Monad.Error ( catchError, runErrorT, throwError, ErrorT )
import Control.Monad.List ( runListT, ListT )
import Control.Monad.State ( evalStateT, get, mzero, put, StateT )
import Control.Monad.Identity ( runIdentity, Identity )
import Data.Either ( lefts, rights )

import Util.OrFail ( orFail )

import Lucretia.Language.Definitions ( Ptr, ErrorMsg )


type CM = StateT CheckerState (ErrorT ErrorMsg Identity)
type CML = ListT CM

evalCM :: CM a -> Either ErrorMsg a
evalCM m = runIdentity $ runErrorT $ evalStateT m initState

evalCML :: CML a -> Either ErrorMsg [a]
evalCML = evalCM . runListT

-- ** Fresh variables (not in wp)
-- New variable names are always fresh, thus there is no need
-- to check for name collisions.

-- | State, needed to serve fresh numbers.
type CheckerState = [Ptr]

initState :: CheckerState
initState = map (\c -> [c]) singleChar ++ map (\n -> "X"++show n) [1..]
  where
  singleChar = ['X'..'Z'] ++ ['A'..'W']

-- | Get fresh 'Ptr'
freshPtr :: CM Ptr
freshPtr = do
  current:next <- get
  put next
  return current

error :: String -> CM a
error = throwError

tryAny :: [CM a] -> CM [a]
tryAny ms = do
  eithers <- sequence $ map toEither ms
  let succeeded = rights eithers
  let failed    = lefts  eithers
  if length succeeded == 0
    then throwError $ if length failed == 1 then head failed
                                            else "No typing path has succeeded: "++show failed
    else return succeeded
    where
    toEither :: CM a -> CM (Either ErrorMsg a)
    toEither m =
      (m >>= return . Right) `catchError` (return . Left)

