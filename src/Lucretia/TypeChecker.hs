-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- TypeChecker runner
-----------------------------------------------------------------------------
--module Lucretia.TypeChecker where
module Lucretia.TypeChecker ( typeProgramme, typeBlock ) where

import Control.Monad.State ( lift )
import Data.Map as Map hiding ( update )

import Util.OrFail ( orFail )

import Lucretia.Language.Definitions
import Lucretia.Language.Syntax
import Lucretia.Language.Types

import Lucretia.TypeChecker.Monad ( CM, evalCM, initState )
import Lucretia.TypeChecker.Rules ( matchBlock )
import Lucretia.TypeChecker.Update ( update, extend )
import Lucretia.TypeChecker.Weakening ( weaker )


typeProgramme :: Defs -> ProgrammeType
typeProgramme b = evalCM (typeProgrammeM b)

typeProgrammeM :: Defs -> CM (IType, Constraints)
typeProgrammeM b = do
  (id, PrePost pre post) <- matchBlock b emptyConstraints
  expectEmptyPreconditionsIn pre
  return (id, post)

    where expectEmptyPreconditionsIn pre =
            (pre == emptyConstraints) `orFail` ("Inside the main programme body a variable was referenced which may be undefined. The preconstraints were: " ++ showConstraints pre)

-- | Function for testing purposes, gets Pre- & Post-Constraints for a given block
typeBlock :: Defs -> Either ErrorMsg Type
typeBlock b = evalCM $ matchBlock b emptyConstraints

