-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Instructions and expressions used in Lucretia language.
-----------------------------------------------------------------------------
module Lucretia.Language.Syntax where

import Lucretia.Language.Definitions (IRec, IVar, IAttr, IType)
import Lucretia.Language.Types (TFun)

type Program = Defs
type Defs = [Def]

data Def
  = SetVar  IVar       Exp
  | SetAttr IVar IAttr Exp

  | If IVar Defs Defs
  | IfHasAttr IVar IAttr Defs Defs

  | Return Exp

  deriving (Eq, Ord, Show)

data Exp
  = EInt    Int
  | EString String
  | EBool   Bool
  | ENone -- ^ () in wp
  | ENew

  | EGetVar  IVar
  | EGetAttr IVar IAttr

  | EFunDef  [IVar] TFun Defs -- f = def (x, y, z) :: (X, Y, Z) [X < {a : Y}, Y < {}, Z < int] -> R [X < {a : Y}, Y < {}, Z < int, R < {b : Z}] { body }
  | EFunCall IVar [IVar] -- f (x, y, z)

  deriving (Eq, Ord, Show)

