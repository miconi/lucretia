{-# LANGUAGE MultiParamTypeClasses, NamedFieldPuns #-}
-- Tiny4.Interpreter with break handling

module Lucretia.Interpreter where
import Lucretia.Definitions (Name, Param)
import Lucretia.Syntax
import Lucretia.Exception

import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Maybe(catMaybes)
import Data.List(intercalate)

data IntState = IntState { 
  store :: Store, 
  freeLocs :: [Loc], 
  env :: Env,
  scopes :: Scopes,
  output :: String}

initState = IntState initStore initFreeLocs initEnv initScopes initOutput

data Exception = ExcBreak Name Val deriving Show

type IM a = ExceptionT Exception (StateT IntState (ErrorT String Identity)) a
-- runIM :: IM a -> IntState -> Either String (a,IntState)
runIM m st = runIdentity (runErrorT (runStateT (runExceptionT m) st))

runProgGetOutput :: Defs -> String
runProgGetOutput p =
  let res = runIM (evalDefs p) initState in
  case res of
    Left e -> "Error: "++e++"\n"
    Right (a, state) ->
      output state++
      case a of
        Left exc -> "Exception: "++show exc++"\n"
        Right _ -> ""

runExp :: Exp -> IO (Val, String)
runExp e = runProg [("_", e)]

--TODO refactor: rename to: runLuProgramPrintingDebugInfo
runProg :: Defs -> IO (Val, String)
runProg p = do
  let res = runIM (evalDefs p) initState
  case res of
    Left e -> return $ (VNone, "Error: "++e++"\n")
    Right (a,state) -> do
      printState state      
      return $ 
        case a of
          Left exc -> (VNone, output state++"Exception: "++show exc++"\n")
          Right ok -> (ok, output state)

    
printState :: IntState -> IO ()
printState state = do
  putStr "Env: "
  printEnv (env state)
  putStr "Store: "
  print $ Map.toAscList (store state)

-- * Values

data Val = VInt Integer | VLoc Loc | VNone | VRec Record
         | VFun Func
         deriving (Eq) 
type Record = (Map.Map Name Val)

instance Show Val where
  show (VInt n) = show n
  show (VLoc l) = "loc:"++show l
  show VNone = "None"
  show (VRec map) =  show $ Map.toAscList map
  show (VFun f) = show f
  
getVInt :: Val -> IM Integer
getVInt (VInt i) = return i
getVInt v = throwError $ "Not an integer: "++show v

getVLoc :: Val -> IM Loc
getVLoc (VLoc l) = return l
getVLoc v = throwError $ "Not a loc: "++show v

getVRec :: Val -> IM Record
getVRec (VRec r) = return r
getVRec v = throwError $ "Not a record: "++show v

getVFun :: Val -> IM Func
getVFun (VFun f) = return f
getVFun v = throwError $ "Not a function: "++show v

isTrueVal :: Val -> Bool
isTrueVal (VInt 0) = False
isTrueVal (VInt _) = True
isTrueVal (VLoc _) = True
isTrueVal (VNone)  = False
isTrueVal (VRec r) = not $ Map.null r
-- * Store    
    
type Loc = Int
type Store = Map.Map Loc Val
initStore :: Store
initStore = Map.empty

alloc :: IM Loc
alloc = do
  locs <- getFreeLocs
  case locs of
    [] -> throwError "alloc: no more free locs"
    (l:ls) -> putFreeLocs ls >> return l

free :: Loc -> IM ()
free l = do 
  ls <- getFreeLocs
  putFreeLocs (l:ls)

updateStore :: Loc -> Val -> IM ()
updateStore v x = modify $ \state -> state { 
  store = Map.insert v x (store state)}

getStore :: IM Store  
getStore = gets store

getLocContents :: Loc -> IM Val
getLocContents loc = do
  store <- getStore 
  let res  = Map.lookup loc store
  maybe (throwError $ "Unknown loc: "++ show loc) return res

initFreeLocs :: [Loc]
initFreeLocs = [1..2^16]

getFreeLocs :: IM [Loc]
getFreeLocs = gets freeLocs

putFreeLocs :: [Loc] -> IM ()
putFreeLocs freeLocs = modify $ \r -> r { freeLocs }

-- * Names and Environment
type Env = Map.Map Name [Loc]  -- a map name -> loc stack
initEnv :: Env
initEnv = Map.empty

printEnv :: Env -> IO ()
printEnv env = print $ Map.toAscList env

type Scope = [Name]
type Scopes = [Scope]

emptyScope = []
initScopes = [emptyScope]

getEnv :: IM Env
getEnv = gets env

putEnv :: Env -> IM ()
putEnv env = modify $ \r -> r { env }

modifyEnv :: (Env -> Env) -> IM ()
modifyEnv f = getEnv >>= putEnv . f

getScopes ::  IM Scopes
getScopes = gets scopes

putScopes :: Scopes -> IM ()
putScopes scopes = modify $ \r -> r { scopes }

modifyScopes :: (Scopes -> Scopes) -> IM ()
modifyScopes f = getScopes >>= putScopes . f

popScope :: IM Scope
popScope = do
  (scope:scopes) <- getScopes
  putScopes scopes
  return scope

enterScope :: IM ()
enterScope = modifyScopes (emptyScope:)

leaveScope :: IM ()
leaveScope = do
  env <- getEnv
  scope <- popScope
  let scopeLocs = catMaybes $ map (flip lookupEnv env) scope
  mapM_ free scopeLocs
  let env' = foldr (\n e -> Map.update pop n e)env scope
  putEnv env' where
    pop :: [Loc] -> Maybe [Loc]
    pop [x] = Nothing
    pop (x:xs) = Just xs

createVar :: Name -> IM Loc
createVar n = do
     l <- alloc
     modifyEnv (updateEnv n l)
     modifyScopes (addLocal n)
     return l

addLocal :: Name -> Scopes -> Scopes
addLocal n (h:t) =(n:h):t 
addLocal n [] = []

lookupEnv :: Name -> Env -> Maybe Loc
lookupEnv n e = do
  stack <- Map.lookup n e 
  case stack of
    [] -> Nothing
    (l:_) -> return l
  
updateEnv :: Name -> Loc -> Env -> Env
updateEnv n l = Map.insertWith (++) n [l] 

getNameLoc :: Name -> IM Loc
getNameLoc n =  do
  env <- getEnv
  let res  = lookupEnv n env
  maybe (throwError $ unwords["Undefined var",n,"env is",show env]) return res
  
getVar :: Name -> IM Val
getVar v =  do
  l <- getNameLoc v
  getLocContents l

-- * Output
initOutput = ""

appendToOutput :: String -> IM ()
appendToOutput s = modify $ \state -> state { 
  output = output state ++ s}

op f e1 e2 = do
  i1 <- getVInt =<< eval e1
  i2 <- getVInt =<< eval e2
  return $ VInt $ f i1 i2

-- | Evaluate expressions
eval :: Exp -> IM Val
eval (EInt i) = return (VInt i)
eval (EVar s) = getVar s
-- liftM2 (+) (eval e1) (eval e2)
eval (EAdd e1 e2) = op (+) e1 e2
eval (EMul e1 e2) = op (*) e1 e2
eval (EIf e1 e2 e3) = do
     v1 <- eval e1
     if isTrueVal v1 then eval e2 else eval e3
eval (ELet "_" e1 e0) = eval e1 >> eval e0
eval (ELet n e1 e0) = do
     enterScope
     execDef n e1
     v0 <- eval e0
     leaveScope
     return v0
eval (ELets ds e0) = do
     enterScope
     execDefs ds
     v0 <- eval e0
     leaveScope
     return v0
eval ENew = do     
     l <- alloc
     updateStore l (VRec Map.empty)
     return $ VLoc l
{-
eval (EDeref e) = do
  v <- eval e
  l <- getVLoc v
  getLocContents l
 -}
eval (EGet n1 n2) = do
   vl <- getVar n1
   l  <- getVLoc vl
   vr <- getLocContents l
   r <- getVRec vr
   maybe (throwError $ unwords [show r,"has no field",n2]) 
         return
         (getFieldVal n2 r)
eval (ESet n1 n2 e) = do
  vl <- getVar n1
  l  <- getVLoc vl
  vr <- getLocContents l
  r <- getVRec vr
  v <- eval e
  updateStore l (VRec (Map.insert n2 v r))
  return v
eval ENone = return VNone
eval (ELabel n t e) = eval e `catchException` handleBreak n where
  handleBreak n (ExcBreak l v) | l == n = return v
  handleBreak n e = throwException e
eval (EBreak l e) = do
  v <- eval e
  throwException (ExcBreak l v)
eval (EFunc f) = return (VFun f)
eval call@(ECall (EVar "print") es) = do
  vs <- mapM eval es
  let line = (intercalate " " . map show) vs
  appendToOutput line
  appendToOutput "\n"
  return VNone
eval call@(ECall e es) = do
  v <- eval e
  f@(Func as _ body) <- getVFun v
  vs <- mapM eval es
  enterScope
  passParams as vs
  r <- eval body
  leaveScope
  return r where
    passParams [] [] = return ()
    passParams as [] = throwError $ "Not enough args in call: "++show call
    passParams [] vs = throwError $ "Too many args in call: "++show call
    passParams (a:as) (v:vs) = do
      l <- createVar a
      updateStore l v
      passParams as vs
      
getFieldVal :: Name -> Record -> Maybe Val
getFieldVal n r= Map.lookup n r

execDef :: Name -> Exp -> IM ()
execDef n e = do
  v <- eval e
  l <- createVar n
  updateStore l v

execDefs :: [(Name,Exp)] -> IM ()
execDefs  [] = return ()
execDefs ((n,e):ds) = execDef n e >> execDefs ds

evalDef :: Name -> Exp -> IM Val
evalDef n e = do
  v <- eval e
  l <- createVar n
  updateStore l v
  return v
  
evalDefs :: [(Name,Exp)] -> IM Val
evalDefs  [(n,e)] = evalDef n e
evalDefs ((n,e):ds) = evalDef n e >> evalDefs ds
