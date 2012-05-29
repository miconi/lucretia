module Lucretia.TypeChecker.Syntax where

import Lucretia.Types
import Lucretia.Definitions (Var, Label, Param)

type Program = Defs
type Defs = [Def]
type Def = (Var, Exp)

data Exp 
    = EInt Integer
    | EBoolTrue
    | EBoolFalse
    | ENone

    | EVar Var
    | ELet Var Exp Exp
    | ELets Defs Exp
    | EIf Exp Exp Exp
    | ENew

    | EGet Var Var
    | ESet Var Var Exp
    | ELabel Label Exp
    | EBreak Label Exp
    | EFunc Func
    | ECall Exp [Exp]

    | EAdd Exp Exp
    | EMul Exp Exp

      deriving (Eq,Show)

data Func = Func [Param] Type Exp deriving (Eq,Show)

instance Num Exp where
    fromInteger = EInt
    (+) = EAdd
    (*) = undefined
    (-) = undefined
    signum = undefined
    abs = undefined

programFromExp :: Exp -> Program
programFromExp e = [("_",e)]

