-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- Basic definitions.
-----------------------------------------------------------------------------
module Lucretia.Language.Definitions (IVar, IAttr, Ptr, ErrorMsg) where

type Identifier = String

type IVar  = Identifier
type IAttr = Identifier
type Ptr = Identifier

type ErrorMsg = String

