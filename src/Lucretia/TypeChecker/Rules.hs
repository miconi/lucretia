-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- TypeChecker rules
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts #-}

module Lucretia.TypeChecker.Rules ( matchProgramme ) where

import Prelude hiding ( error )
import Control.Monad.State ( lift )
import Data.Map as Map hiding ( update )
import Data.Set as Set
import Data.Function ( on )

import Util.Debug ( traceShowIdHlWith )
import Util.OrFail ( orFail )

import Lucretia.Language.Definitions
import Lucretia.Language.Syntax
import Lucretia.Language.Types

import Lucretia.TypeChecker.Monad ( error, freshPtr, CM )
import Lucretia.TypeChecker.Monad ( freshPtr, CM )
import Lucretia.TypeChecker.Renaming ( applyRenaming, freeVariables, getRenamingOnEnv, getRenaming, getRenamingPP, Renaming, RenamingType(..) )
import Lucretia.TypeChecker.Update ( merge, update )
import Lucretia.TypeChecker.Weakening ( checkWeaker, weaker )


matchProgramme :: Block -> CM Type
matchProgramme ss = bindBlockCs emptyConstraints ss

bindBlockCs :: Constraints -> Block -> CM Type
bindBlockCs cs = bindBlock (undefinedId, PrePost emptyConstraints cs)

-- | Bind Pre- & Post-Constraints 'pp' of a code block B with Type of the next statement x, producing Type of the whole Block including statement x.
-- B x1
-- B x2
-- B ...
-- B xN
--   x
bindBlock :: Type -> Block -> CM Type -- TODO OPT RTR Ptr to (Maybe Ptr)
bindBlock t [] = return t
bindBlock (_, pp) (s:ss) = do
  sT <- bindStmt pp s
  bindBlock sT ss

bind :: PrePost -> Type -> CM Type
bind ppCall (iDecl, ppDecl) = do
  renaming      <- getRenamingOnEnv (BindRenaming $ freshVariables ppCall) (_post ppCall) (_pre ppDecl)
  let ppRenamed  = applyRenaming renaming ppDecl

  toWeaken      <- _post ppCall `weaker` _pre ppRenamed
  -- Using Monotonicity (Principle 1 in paper) here:
  preMerged     <- toWeaken `update` _pre ppCall
  -- Q why not:    toWeaken `update` _post ppCall
  -- A toWeaken is already inside _post ppRenamed
  -- because: toWeaken `isSubsetOf` _pre ppRenamed && _pre ppRenamed `isSubsetOf` _post ppRenamed
  postMerged    <- _post ppCall `update` _post ppRenamed
  
  return ( applyRenaming renaming iDecl
         , PrePost preMerged postMerged
         )

  where
  freshVariables :: PrePost -> Set Ptr
  freshVariables pp = (Set.difference `on` freeVariables) (_post pp) (_pre pp)

bindPP :: PrePost -> PrePost -> CM Type
bindPP ppCall ppDecl = bind ppCall (undefinedId, ppDecl)

bindStmt :: PrePost -> Stmt -> CM Type

-- Binding statements inside an if statement is handled in a special way. Other statements are handled by matchStmt function.
-- TODO If can be an expression
bindStmt pp (If x b1 b2) = do
  xId <- freshPtr
  t@(_, PrePost _ cs) <- bindPP pp (isBool x xId)

  t1 <- bindBlock t b1
  t2 <- bindBlock t b2

  mergeTypes cs t1 t2

-- bindStmt pp (IfHasAttr x a b1 b2) = do
--   t1 <- matchProgramme b1 cs
--   t2 <- matchProgramme b2 cs
--   branchesMerged <- mergeTypes cs t1 t2
-- 
--   xId <- freshPtr
--   bind (isBool x xId) branchesMerged
-- 
--   where 
--   thenStarter :: IVar -> IAttr -> PrePost
--   elseStarter :: IVar -> IAttr -> PrePost

-- Type produced by compiling all the other statements to PrePost can be bound in the same way.
bindStmt pp s = do
  sT <- matchStmt pp s
  bind pp sT

isBool :: IVar -> Ptr -> PrePost
isBool x xId = PrePost cs cs
  where cs = Map.fromList [ toSingletonRec env x xId
                          , xToBool
                          ]
        xToBool = (xId, tOrPrimitive KBool)

mergeTypes :: Constraints -> Type -> Type -> CM Type
mergeTypes cs (_, pp1) (_, pp2) = do
  renaming <- getRenamingPP FullRenaming pp1 pp2
  renamingOnlyOnVariablesCreatedInScope cs renaming
  let pp2Renamed = applyRenaming renaming pp2
  mergedPP <- merge pp1 pp2Renamed
  return (undefinedId, mergedPP)
    where
    renamingOnlyOnVariablesCreatedInScope :: Constraints -> Renaming -> CM ()
    renamingOnlyOnVariablesCreatedInScope cs r =
      (freeVariables cs `Set.intersection` freeVariables r == Set.empty)
      `orFail` "Cannot merge type pointers from 'then' and 'else' branches of an 'if' instruction. Cannot merge fresh type pointer (i.e. created in a branch) with a stale type pointer (i.e. created before the branch). Only type pointers freshly created in both branches can be merged (i.e. one created in 'then', the other in 'else')."
-- All @Ptr@ variables in the returned @Type@ must be fresh, so there is no risk of @Ptr@ name clashes when @'bind'ing@ @Type@ of the current @Stmt@ to the @PrePost@ of the block preceding the @Stmt@.
matchStmt :: PrePost -> Stmt -> CM Type

matchStmt pp (Return e) = matchExpFresh pp e

matchStmt pp (SetVar x e) = do
  (eId, ePP) <- matchExpFresh pp e
  let setPP = setVar x eId
  bind ePP (eId, setPP)

  --HERE why binding & renaming works? G make sure it works
  -- (eId, ePP) <- matchAndRename
  -- let setPP = setVar x eId
  -- bind (Set.singleton eId) ePP (eId, setPP)

  where setVar x eId = PrePost pre post
          where pre  = Map.fromList [ toEmptyRec     env       ]
                post = Map.fromList [ toSingletonRec env x eId ]

matchStmt pp (SetAttr x a e) = do
  (eId, ePP) <- matchExpFresh pp e
  xId <- freshPtr
  let setPP = setAttr x xId a eId
  bind ePP (eId, setPP)

  where setAttr x xId a eId = PrePost pre post
          where pre  = Map.fromList [ toSingletonRec env x xId
                                    , toEmptyRec     xId
                                    ]
                post = Map.fromList [ toSingletonRec env x xId
                                    , toSingletonRec xId a eId
                                    ]


-- | 'match' rules to an @Exp@ producing Type, then rename all @Ptrs@ to fresh variables in that type.
matchExpFresh :: PrePost -> Exp -> CM Type
matchExpFresh pp e = do
  t <- matchExp pp e
  renameToFresh t

    where
    renameToFresh :: Type -> CM Type
    renameToFresh t@(_, pp) = do
      let usedPtrs = Set.toList $ freeVariables pp
      renaming <- mapM usedToFresh usedPtrs
      return $ applyRenaming (Set.fromList renaming) t

    usedToFresh :: Ptr -> CM (Ptr, Ptr)
    usedToFresh iUsed = do iFresh <- freshPtr
                           return (iFresh, iUsed)
  
-- | Get Type (Pre- & Post-Constraints) of the next expression e. Type of a preceding block of statements B is given, so that the rule for a function call can read a called function signature.
-- B e1
-- B e2
-- B ...
-- B eN
--   e
matchExp :: PrePost -> Exp -> CM Type

matchExp _ (EGetVar a) = return (aId, PrePost constraints constraints)
  where constraints = Map.fromList [ toSingletonRec env a aId ]

matchExp _ (EGetAttr x a) = return (aId, PrePost constraints constraints)
  where constraints = Map.fromList [ toSingletonRec env x xId
                                   , toSingletonRec xId a aId
                                   ]

matchExp _ (EInt _)    = postPointerPrimitive KInt
matchExp _ (EString _) = postPointerPrimitive KString
matchExp _ (EBool _)   = postPointerPrimitive KBool
matchExp _  ENone      = postPointerPrimitive KNone
matchExp _  ENew       = postPointer tOrEmptyRec

matchExp (PrePost _ cs) (EFunCall f xsCall) = do
  -- Pre- & post- constraints must be declared in the code.
  -- In case of other function signatures, InheritedPP should be
  -- replaced in matchExp (EFunDef ...).
  TFunSingle tsDecl iDecl (DeclaredPP ppDecl) <- getFunType f cs
  checkArgsLength xsCall tsDecl
  let ppInherited = inheritPP ppDecl
      -- Q how it should work

  let ppWithArgs = addArgsPP xsCall tsDecl ppInherited
  return (iDecl, ppWithArgs)

    where
    getFunType :: IVar -> Constraints -> CM TFunSingle
    getFunType f cs =
      case f `lookupInEnv` cs of
        Nothing                      -> error pleaseDefineSignature
        Just Forbidden               -> error pleaseDefineSignature
        Just (WithPtr Optional _   ) -> error $ "Function "++f++" may be undefined."
        Just (WithPtr Required ifun) -> case ifun `lookupInConstraints` cs of
           Just tfun -> unwrapFunFromOr tfun
           Nothing   -> error pleaseDefineSignature 
        where pleaseDefineSignature = "Please declare signature for the function "++f++". Infering type of a function passed as a parameter to another function (higher order function type inference) is not supported yet."

    unwrapFunFromOr :: TOr -> CM TFunSingle
    unwrapFunFromOr tOr =
      case Map.toList tOr of
        [(KFun, TFun tfun)] -> return tfun
        otherwise           -> error $ "Variable "++f++" should be a function."

    -- | Check length of arguments,
    -- should be the same in the declaration and the place of call
    checkArgsLength xsCall tsDecl =
      (length xsCall == length tsDecl) `orFail`
        ("Function "++f++" is applied to "++show (length xsCall)++" argument(s), but "++show (length tsDecl)++" argument(s) should be provided.\n")


-- TODO implement TFunOr
matchExp _ (EFunDef argNames maybeSignature funBody) = do
  funType <- case maybeSignature of
    Just signature -> checkSignature signature
    Nothing        -> inferSignature argNames funBody
  postPointer $ tOrFromTFunSingle funType

  where
    checkSignature :: TFunSingle -> CM TFunSingle
    checkSignature decl@(TFunSingle argTypes iDecl (DeclaredPP ppDecl)) = do
      -- pre- & post- constraints must be declared in the code
      -- that should be checked by Lucretia parser
      -- case maybePPDecl of
      --   InheritedPP -> error $ "Containig function must declare pre- and post-constraints."
      --   DeclaredPP ppDecl ->
      --     do
      --
      -- We are adding pre-constraints from the function signature
      -- to make available at the call site the signatures
      -- of the functions passed as parameters
      let ppInherited = inheritPP ppDecl
      let argCs = addArgsCs argNames argTypes (_pre ppInherited)

      (iInfered, ppInfered) <- bindBlockCs argCs funBody

      checkEmptyPreEnv ppInfered
      let ppInferedNoEnv = eraseEnv ppInfered

      checkPre ppInherited ppInferedNoEnv
      checkPost argNames iInfered (_post ppInferedNoEnv) iDecl (_post ppInherited)

      -- TODO clean constraints
      return decl

    checkPre = checkWeaker `on` _pre

    checkPost :: [Ptr] -> Ptr -> Constraints -> Ptr -> Constraints -> CM ()
    checkPost argNames iInfered csInfered iDecl csDecl = do
      renaming <- getRenaming FullRenaming (iDecl:argNames, csDecl) (iInfered:argNames, csInfered)
      (iDecl == applyRenaming renaming iInfered) `orFail`
        ("Returned type of a function was declared "++iDecl++" but it is "++iInfered)
      applyRenaming renaming csInfered `checkWeaker` csDecl
            

    inferSignature :: [IVar] -> Block -> CM TFunSingle
    inferSignature argNames funBody = do
      funType <- matchProgramme funBody

      let argTypes = fmap (\n -> "A"++n) argNames
      let argCs = addArgsCs argNames argTypes emptyConstraints

      (funReturnId, funBodyPP) <- bind (PrePost emptyConstraints argCs) funType

      checkEmptyPreEnv funBodyPP
      let funBodyNoEnvPP = eraseEnv funBodyPP

      -- TODO clean constraints
      return $ TFunSingle argTypes funReturnId (DeclaredPP funBodyNoEnvPP)

    -- | Checks that no variable was referenced, apart from the arguments
    checkEmptyPreEnv :: PrePost -> CM ()
    checkEmptyPreEnv pp = (getEnv (_pre pp) == emptyRec) `orFail` "Inside a function body a variable was referenced which may be undefined and is not in the function parameters."

    eraseEnv :: PrePost -> PrePost
    eraseEnv (PrePost pre post) = PrePost (eraseEnv' pre) (eraseEnv' post)
    eraseEnv' :: Constraints -> Constraints
    eraseEnv' = Map.delete env

addArgsPP :: [IAttr] -> [Ptr] -> PrePost -> PrePost
addArgsPP argNames argTypes (PrePost pre post) =
  PrePost
    (addArgsCs argNames argTypes pre)
    (addArgsCs argNames argTypes post)

addArgsCs :: [IVar] -> [Ptr] -> Constraints -> Constraints
addArgsCs argNames argTypes = Map.insert env (argsTOr argNames argTypes)

argsTOr :: [IVar] -> [Ptr] -> TOr
argsTOr argNames argTypes = tOrFromTRec $ Map.fromList $ zip argNames (requiredList argTypes)


postPointerPrimitive :: Kind -> CM Type
postPointerPrimitive kind = postPointer $ tOrPrimitive kind

postPointer :: TOr -> CM Type
postPointer tOr = return (xId, PrePost emptyConstraints (Map.insert xId tOr emptyConstraints))

required :: Ptr -> TAttr
required i = (WithPtr Required i)

requiredList :: [Ptr] -> [TAttr]
requiredList = fmap required

inheritPP :: PrePost -> PrePost
inheritPP pp = inherit pp pp
class InheritPP a where
  inherit :: PrePost -> a -> a
instance InheritPP PrePost where
  inherit pp (PrePost pre post) = PrePost (inherit pp pre) (inherit pp post)
instance InheritPP Constraints where
  inherit pp = Map.map $ inherit pp
instance InheritPP TOr where
  inherit pp = Map.map $ inherit pp
instance InheritPP TSingle where
  inherit pp (TFun tfun) = TFun $ inherit pp tfun
  inherit pp other = other
instance InheritPP TFunSingle where
  inherit pp (TFunSingle funArgs funRet funPP) = TFunSingle funArgs funRet $ inherit pp funPP
instance InheritPP FunPrePost where
  inherit pp InheritedPP = DeclaredPP pp
  inherit pp other = other


