-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- TypeChecker rules
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts #-}

module Lucretia.TypeChecker.Rules ( bindBlock ) where

import Prelude hiding ( error, sequence )
import Data.Traversable ( sequence )

import Control.Monad.List ( runListT, ListT, ListT(..) )
import Control.Monad.State ( lift )
import Data.Map as Map hiding ( update )
import Data.Set as Set
import Data.Function ( on )

import Util.Debug ( traceShowIdHl, traceShowIdHlWith )
import Util.OrFail ( orFail )

import Lucretia.Language.Definitions
import Lucretia.Language.Syntax
import Lucretia.Language.Types

import Lucretia.TypeChecker.Monad ( error, freshPtr, tryAny, CM )
import Lucretia.TypeChecker.Renaming ( applyRenaming, freeVariables, getRenamingOnEnv, getRenaming, getRenamingPP, Renaming, RenamingType(..) )
import Lucretia.TypeChecker.Update ( merge, update )
import Lucretia.TypeChecker.Weakening ( funDefWeaker, funAppWeaker )

liftList :: (Monad m) => [a] -> ListT m a
liftList = ListT . return

bindBlock :: Block -> CM Types
bindBlock ss = bindCsBlock emptyConstraints ss

bindCsBlock :: Constraints -> Block -> CM Types
bindCsBlock cs = bindTypeBlock (undefinedId, PrePost emptyConstraints cs)
--bindCsBlock css = bindTypeBlock [(undefinedId, PrePost emptyConstraints cs) | cs <- css]

-- In all binding functions a following invariant is preserved:
-- error means that all of the possible typing paths have failed.

-- | Bind Pre- & Post-Constraints 'pp' of a code block B with Type of the next statement x, producing Type of the whole Block including statement x.
-- B x1
-- B x2
-- B ...
-- B xN
--   x
bindTypeBlock :: Type -> Block -> CM Types
bindTypeBlock t [] = return [t]
bindTypeBlock (_, pp) (s:ss) = do
  ts <- bindStmt pp s
  fmap concat $ tryAny [bindTypeBlock t ss | t <- ts]

bind :: PrePost -> Type -> CM Type
bind ppCall (iDecl, ppDecl) = do
  let fresh      = freshPtrs ppCall
  renaming      <- getRenamingOnEnv (BindRenaming fresh) (_post ppCall) (_pre ppDecl)
  let ppRenamed  = applyRenaming renaming ppDecl

  toWeaken      <- funAppWeaker fresh (_post ppCall) (_pre ppRenamed)
  -- Using Monotonicity (Principle 1 in paper) here:
  preMerged     <- toWeaken `update` _pre ppCall
  -- Q why not:    toWeaken `update` _post ppCall
  -- A toWeaken is already inside _post ppRenamed
  -- because: toWeaken `isSubsetOf` _pre ppRenamed && _pre ppRenamed `isSubsetOf` _post ppRenamed
  postMerged    <- _post ppCall `update` _post ppRenamed
  
  return ( applyRenaming renaming iDecl
         , PrePost preMerged postMerged
         )

freshPtrs :: PrePost -> Set Ptr
freshPtrs pp = (Set.difference `on` freeVariables) (_post pp) (_pre pp)

bindPP :: PrePost -> PrePost -> CM Type
bindPP ppCall ppDecl = bind ppCall (undefinedId, ppDecl)

bindStmt :: PrePost -> Stmt -> CM Types

-- Binding statements inside an if statement is handled in a special way. Other statements are handled by matchStmt function.
bindStmt pp (If x b1 b2) = do
  xId <- freshPtr
  t@(_, PrePost _ cs) <- bindPP pp (isBool x xId)

  t1 <- bindTypeBlock t b1
  t2 <- bindTypeBlock t b2

  mergeTypes cs t1 t2

bindStmt pp@(PrePost _ cs) (IfHasAttr x a b1 b2) = fmap concat $
  tryAny [ ifHasAttrBoth  pp x a b1 b2
         , ifHasAttrPlus  pp x a b1
         , ifHasAttrMinus pp x a    b2
         ]

-- Type produced by compiling all the other statements to PrePost can be bound in the same way.
bindStmt pp s = do
  ts <- matchStmt pp s
  tryAny [bind pp t | t <- ts]

ifHasAttrBoth  :: PrePost -> IVar -> IAttr -> Block -> Block -> CM Types
ifHasAttrPlus  :: PrePost -> IVar -> IAttr -> Block -> CM Types
ifHasAttrMinus :: PrePost -> IVar -> IAttr -> Block -> CM Types

ifHasAttrBoth pp@(PrePost _ cs) x a b1 b2 = do
  xId <- freshPtr
  aId <- freshPtr

  t1  <- bindPP pp (thenStarter x xId a aId)
  t2  <- bindPP pp (elseStarter x xId a aId)

  t1' <- bindTypeBlock t1 b1
  t2' <- bindTypeBlock t2 b2

  mergeTypes cs t1' t2'

  where 
  thenStarter :: IVar -> Ptr -> IAttr -> Ptr -> PrePost
  thenStarter x xId a aId = PrePost (starterOptional  x xId a aId)
                                    (starterRequired  x xId a aId)
  elseStarter :: IVar -> Ptr -> IAttr -> Ptr -> PrePost
  elseStarter x xId a aId = PrePost (starterOptional  x xId a aId)
                                    (starterForbidden x xId a aId)

type Starter = IVar -> Ptr -> IAttr -> Ptr -> Constraints
starterRequired, starterOptional, starterForbidden :: Starter

starterOptional  x xId a aId =
  Map.fromList [ toSingletonRec env x xId
               , (xId, tOrFromTRec $ Map.singleton a (Optional aId))
               ]
starterRequired  x xId a aId =
  Map.fromList [ toSingletonRec env x xId
               , (xId, tOrFromTRec $ Map.singleton a (Required aId))
               ]
starterForbidden x xId a aId =
  Map.fromList [ toSingletonRec env x xId
               , (xId, tOrFromTRec $ Map.singleton a  Forbidden    )
               ]

ifHasAttrPlus  = ifHasAttrStar starterRequired  
ifHasAttrMinus = ifHasAttrStar starterForbidden

ifHasAttrStar :: Starter -> PrePost -> IVar -> IAttr -> Block -> CM Types
ifHasAttrStar starter pp@(PrePost _ cs) x a b = do
  xId <- freshPtr
  aId <- freshPtr
  t   <- bindPP pp (PrePost (starter x xId a aId) (starter x xId a aId))
  bindTypeBlock t b

isBool :: IVar -> Ptr -> PrePost
isBool x xId = PrePost cs cs
  where cs = Map.fromList [ toSingletonRec env x xId
                          , xToBool
                          ]
        xToBool = (xId, tOrPrimitive KBool)

mergeTypes :: Constraints -> Types -> Types -> CM Types
mergeTypes cs ts1 ts2 = tryAny [mergeTypesSingle cs t1 t2 |  t1 <- ts1, t2 <- ts2]

mergeTypesSingle :: Constraints -> Type -> Type -> CM Type
mergeTypesSingle cs (_, pp1) (_, pp2) = do
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
matchStmt :: PrePost -> Stmt -> CM Types

matchStmt pp (Return e) = matchExpFresh pp e

matchStmt pp (SetVar x e) = do
  ts <- matchExpFresh pp e
  tryAny [let setPP = (setVar x eId) in
                bind ePP (eId, setPP)
                | (eId, ePP) <- ts]

  --HERE why binding & renaming works? G make sure it works
  -- (eId, ePP) <- matchAndRename
  -- let setPP = setVar x eId
  -- bind (Set.singleton eId) ePP (eId, setPP)

  where setVar x eId = PrePost pre post
          where pre  = Map.fromList [ toEmptyRec     env       ]
                post = Map.fromList [ toSingletonRec env x eId ]

matchStmt pp (SetAttr x a e) = do
  xId <- freshPtr
  ts <- matchExpFresh pp e
  tryAny [let setPP = setAttr x xId a eId in
                bind ePP (eId, setPP)
                | (eId, ePP) <- ts]

  where setAttr x xId a eId = PrePost pre post
          where pre  = Map.fromList [ toSingletonRec env x xId
                                    , toEmptyRec     xId
                                    ]
                post = Map.fromList [ toSingletonRec env x xId
                                    , toSingletonRec xId a eId
                                    ]


-- | 'match' rules to an @Exp@ producing Type, then rename all @Ptrs@ to fresh variables in that type.
matchExpFresh :: PrePost -> Exp -> CM Types
matchExpFresh pp e = do
  ts <- matchExp pp e
  tryAny [renameToFresh t | t <- ts]

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
matchExp :: PrePost -> Exp -> CM Types

matchExp _ (EGetVar a) = return [(aId, PrePost constraints constraints)]
  where constraints = Map.fromList [ toSingletonRec env a aId ]

matchExp _ (EGetAttr x a) = return [(aId, PrePost constraints constraints)]
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
  ts <- getFunType f cs
  tryAny [callSingleFun cs t | t <- ts]

    where
    callSingleFun :: Constraints -> TFunSingle -> CM Type
    callSingleFun cs (TFunSingle tsDecl iDecl (DeclaredPP ppDecl)) = do
      checkArgsLength xsCall tsDecl
      let ppInherited = inheritPP ppDecl
      let ppWithArgs = addArgsPP xsCall tsDecl ppInherited
      return (iDecl, ppWithArgs)

    getFunType :: IVar -> Constraints -> CM TFun
    getFunType f cs =
      case f `lookupInEnv` cs of
        Nothing                      -> error pleaseDefineSignature
        Just Forbidden               -> error pleaseDefineSignature
        Just (Optional _   ) -> error $ "Function "++f++" may be undefined."
        Just (Required ifun) -> case ifun `lookupInConstraints` cs of
           Just tfun -> unwrapFunFromOr tfun
           Nothing   -> error pleaseDefineSignature 
        where pleaseDefineSignature = "Please declare signature for the function "++f++". Infering type of a function passed as a parameter to another function (higher order function type inference) is not supported yet."

    unwrapFunFromOr :: TOr -> CM TFun
    unwrapFunFromOr tOr =
      case Map.toList tOr of
        [(KFun, TFun tfun)] -> return tfun
        otherwise           -> error $ "Variable "++f++" should be a function."

    -- | Check length of arguments,
    -- should be the same in the declaration and the place of call
    checkArgsLength xsCall tsDecl =
      (length xsCall == length tsDecl) `orFail`
        ("Function "++f++" is applied to "++show (length xsCall)++" argument(s), but "++show (length tsDecl)++" argument(s) should be provided.\n")


matchExp _ (EFunDef argNames maybeSignature funBody) = do
  funType <- case maybeSignature of
    []        -> inferSignature funBody argNames
    signature -> checkSignature funBody signature
  postPointer $ tOrFromTFun funType

  where
    -- All declared signatures must be valid, so we use mapM instead of tryAny.
    checkSignature :: Block -> TFun -> CM TFun
    checkSignature funBody = mapM $ checkSingleSignature funBody

    checkSingleSignature :: Block -> TFunSingle -> CM TFunSingle
    checkSingleSignature funBody decl = do
      -- pre- & post- constraints must be declared in the code
      -- that should be checked by Lucretia parser
      -- case maybePPDecl of
      --   InheritedPP -> error $ "Containig function must declare pre- and post-constraints."
      --   DeclaredPP ppDecl ->
      --     do

      let TFunSingle argTypes iDecl (DeclaredPP ppDecl) = renameToUnique decl

      -- We are adding pre-constraints from the function signature to make
      -- signatures of the functions passed as parameters available at their
      -- call site.
      let ppInherited = inheritPP ppDecl
      let argCs = addArgsCs argNames argTypes (_pre ppInherited)

      inferedTs <- bindCsBlock argCs funBody
      tryAny [checkFunConditions argTypes iDecl ppInherited t | t <- inferedTs]

      return decl

        where
        checkFunConditions :: [Ptr] -> Ptr -> PrePost -> Type -> CM ()
        checkFunConditions argTypes iDecl ppInherited (iInfered, ppInfered) = do
          checkEmptyPreEnv ppInfered
          let ppInferedNoEnv = eraseEnv ppInfered

          checkPre ppInferedNoEnv
          let fresh = freshPtrs ppInfered
          checkPost fresh argTypes
                          iInfered (_post ppInferedNoEnv)
                          iDecl (_post ppInherited)

        -- | Add a prefix "S" to all the type variables so that their names do
        -- not clash with the fresh variables used later on.
        renameToUnique :: TFunSingle -> TFunSingle
        renameToUnique t =
          let usedPtrs = freeVariables t in
          let renaming = Set.map (\id -> ("S"++id, id)) usedPtrs in
          applyRenaming renaming t

    checkPre ppInferedNoEnv = (_pre ppInferedNoEnv == Map.empty) `orFail` ("There were following references to attributes undefined in the preconstraints in the declared signature of a function: "++(showConstraints $ _pre ppInferedNoEnv))

    checkPost :: Set Ptr -> [Ptr] -> Ptr -> Constraints -> Ptr -> Constraints -> CM ()
    checkPost fresh argTypes iInfered csInfered iDecl csDecl = do
      renaming <- getRenaming FullRenaming
                              (iDecl   :argTypes, csDecl   )
                              (iInfered:argTypes, csInfered)
      (iDecl == applyRenaming renaming iInfered)
        `orFail` ("Returned type of a function was declared "++iDecl++" but it is "++iInfered)
      funDefWeaker fresh (applyRenaming renaming csInfered) csDecl

    inferSignature :: Block -> [IVar] -> CM TFun
    inferSignature funBody argNames = do
      inferedTs <- bindBlock funBody

      -- When infering a function signature we only allow a situation when all
      -- function parameters have unique type pointers. Still a programmer can
      -- declare a function signature where for some two different parameters
      -- their type pointers are the same.
      let argTypes = fmap (\n -> "A"++n) argNames
      let argCs = addArgsCs argNames argTypes emptyConstraints

      tryAny [checkFunConditions argTypes argCs t | t <- inferedTs]

        where
        checkFunConditions :: [Ptr] -> Constraints -> Type -> CM TFunSingle
        checkFunConditions argTypes argCs inferedT = do
          (funReturnId, funBodyPP) <- bind (PrePost emptyConstraints argCs) inferedT

          checkEmptyPreEnv funBodyPP
          let funBodyNoEnvPP = eraseEnv funBodyPP

          -- Here we could clean the post-constraints from the garbage variables
          -- created inside the function body (they do not add anything to the
          -- function signature and they only clutter the infered signature) but
          -- there is no garbage collection for variables defined in the Lucretia
          -- type system.

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


postPointerPrimitive :: Kind -> CM Types
postPointerPrimitive kind = postPointer $ tOrPrimitive kind

postPointer :: TOr -> CM Types
postPointer tOr = return [(xId, PrePost emptyConstraints (Map.insert xId tOr emptyConstraints))]

required :: Ptr -> TAttr
required i = (Required i)

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
instance InheritPP TFun where
  inherit pp = fmap (inherit pp)
instance InheritPP TFunSingle where
  inherit pp (TFunSingle funArgs funRet funPP) = TFunSingle funArgs funRet $ inherit pp funPP
instance InheritPP FunPrePost where
  inherit pp InheritedPP = DeclaredPP pp
  inherit pp other = other


