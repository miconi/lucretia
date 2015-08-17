-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- TypeChecker runner
-----------------------------------------------------------------------------
--module Lucretia.TypeChecker where
module Lucretia.TypeChecker ( typeProgramme, typeBlock ) where

import Util.OrFail ( orFail )

import Lucretia.Language.Definitions
import Lucretia.Language.Syntax
import Lucretia.Language.Types

import Lucretia.TypeChecker.Monad ( CM, evalCM, initState )
import Lucretia.TypeChecker.Rules ( matchProgramme )


typeProgramme :: Block -> ProgrammeType
typeProgramme b = evalCM (typeProgrammeM b)

typeProgrammeM :: Block -> CM (Ptr, Constraints)
typeProgrammeM b = do
  (id, PrePost pre post) <- matchProgramme b
  expectEmptyPreconditionsIn pre
  return (id, post)

    where expectEmptyPreconditionsIn pre =
            (pre == emptyConstraints) `orFail` ("Inside the main programme body a variable was referenced which may be undefined. The preconstraints were: " ++ showConstraints pre)

-- | Function for testing purposes, gets Pre- & Post-Constraints for a given block
typeBlock :: Block -> Either ErrorMsg Type
typeBlock b = evalCM $ matchProgramme b

