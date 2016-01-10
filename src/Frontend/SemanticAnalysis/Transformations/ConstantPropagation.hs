{-# LANGUAGE GADTs #-}
module Frontend.SemanticAnalysis.Transformations.ConstantPropagation where

import Prelude hiding ( id )
import Control.Monad.State
import Control.Monad.Except
import Control.Lens hiding ( op, Empty )
import qualified Data.Map as M

import Frontend.SemanticAnalysis.CheckError
import Frontend.SemanticAnalysis.Monad
import Frontend.Parser.AbsLatte


data Value
  = IntV     Int
  | StringV  String
  | BooleanV Bool
  | Dynamic
  deriving Eq

type ValEnv = M.Map Ident Value

data CPState
  = CPState [ValEnv] RunMode

instance HasRunMode CPState where
  runMode = lens (\(CPState _ rm) -> rm) (\(CPState a _) rm -> CPState a rm)

valEnv :: Lens' CPState [ValEnv]
valEnv = lens (\(CPState ve _) -> ve) (\(CPState _ rm) ve -> CPState ve rm)

type CPM a
  = CheckM' CPState a

defaultValue :: Type -> Value
defaultValue t = case t of
  IntT     -> IntV     0
  BooleanT -> BooleanV False
  StringT  -> StringV  ""
  VoidT    -> error "Void has no default value"
  _        -> Dynamic -- TODO object values?

initialState :: CPState
initialState = CPState [M.empty] Normal

propagateConstants :: Tree a -> CheckM (Tree a)
propagateConstants x = do
  rm <- use runMode
  res <- liftIO $ runExceptT (evalStateT (doCP x) (initialState & runMode .~ rm))
  case res of
    Left err -> throwError err
    Right x' -> return x'

doCP :: Tree a -> CPM (Tree a)
doCP = composOpM doCPInner

newScope :: CPM a -> CPM a
newScope m = do
  outerEnv <- use valEnv
  valEnv   %= (M.empty:)
  res      <- m
  valEnv   .= outerEnv
  return res

declVar :: Ident -> CPM ()
declVar ident =
  valEnv %= (\(m:t) -> (M.insert ident Dynamic m):t)

getVar :: Ident -> CPM Value
getVar ident = do
  currentEnv <- use valEnv
  return $ getVarInner currentEnv ident

getVarInner :: [ValEnv] -> Ident -> Value
getVarInner (h:t) ident =
  case M.lookup ident h of
    Just val -> val
    Nothing  -> getVarInner t ident
getVarInner _ _ = error "Constant Propagation: cannot find variable"

setVar :: (Ident, Value) -> CPM ()
setVar (ident, val) =
  valEnv %= setVarInner ident val

setVarInner :: Ident -> Value -> [ValEnv] -> [ValEnv]
setVarInner ident val (h:t) =
  case M.lookup ident h of
    Just _   -> (M.insert ident val h):t
    Nothing  -> h:(setVarInner ident val t)
setVarInner _ _ _ = error "Constant Propagation: cannot find variable"

evalItem :: Type -> Item -> CPM (Ident, Value, Item)
evalItem t item@(NoInit ident) = return (ident, defaultValue t, item)

evalItem _ (Init ident e1) = do
  (val, e1') <- evalExpr e1
  return (ident, val, Init ident e1')

doCPInner :: Tree a -> CPM (Tree a)
doCPInner x = case x of
  ClsDef{} -> objectsNotSupportedYet
  FnDef _ _ args _ -> newScope $ do
    mapM_ declVar [ ident | Arg _ ident <- args ]
    doCP x
  Ret e1 -> do
    (_, e1') <- evalExpr e1
    return $ Ret e1'
  BStmt _  -> newScope $ doCP x
  Decl typ items -> do
    initItems <- mapM (evalItem typ) items
    mapM_ declVar     [  a    | (a,_,_) <- initItems ]
    mapM_ setVar      [ (a,b) | (a,b,_) <- initItems ]
    return $ Decl typ [  c    | (_,_,c) <- initItems ]
  Ass (LVar ident) e1 -> do
    (val, e1') <- evalExpr e1
    setVar (ident, val)
    return $ Ass (LVar ident) e1'
  Ass _ _ -> objectsNotSupportedYet
  Incr (LVar ident) -> do
    val <- getVar ident
    case val of
      IntV n -> setVar (ident, IntV (n+1))
      _      -> return ()
    return x
  Incr _ -> objectsNotSupportedYet
  Decr (LVar ident) -> do
    val <- getVar ident
    case val of
      IntV n -> setVar (ident, IntV (n-1))
      _      -> return ()
    return x
  Decr _ -> objectsNotSupportedYet
  Cond e1 s1 -> do
    (val, e1') <- evalExpr e1
    case val of
      BooleanV False -> return Empty
      BooleanV True  -> doCP s1
      _              -> doCP (Cond e1' s1)
  CondElse e1 s1 s2 -> do
    (val, e1') <- evalExpr e1
    case val of
      BooleanV False -> doCP s2
      BooleanV True  -> doCP s1
      _              -> doCP (CondElse e1' s1 s2)
  For _ _ _ _ -> arraysNotSupportedYet
  SExp e1 -> do
    (_, e1') <- evalExpr e1
    return $ SExp e1'
  _  -> doCP x

simplify :: Expr -> Value -> Expr
simplify e1 v = case v of
  IntV n     -> ELitInt (fromIntegral n)
  StringV s  -> EString s
  BooleanV b -> if b then ELitTrue else ELitFalse
  Dynamic    -> e1 -- cannot simplify, return original expression

evalExpr :: Expr -> CPM (Value, Expr)
evalExpr x = do
  (val, origExpr) <- evalExprInner x
  let simplExpr = simplify origExpr val
  return (val, simplExpr)

evalExprInner :: Expr -> CPM (Value, Expr)
evalExprInner x = let nc v = return (v, x) in case x of
  ELitInt n         -> nc $ IntV (fromIntegral n)
  ELitTrue          -> nc $ BooleanV True
  ELitFalse         -> nc $ BooleanV False
  ELitNull _        -> objectsNotSupportedYet
  EString s         -> nc $ StringV s
  EApp {}           -> nc $ Dynamic
  ELVal (LVar id)   -> do
    val <- getVar id
    nc val
  ELVal (LArrAcc{}) -> arraysNotSupportedYet
  ELVal (LClsAcc{}) -> objectsNotSupportedYet
  ClsApply{}        -> objectsNotSupportedYet
  ClsAlloc{}        -> objectsNotSupportedYet
  ArrAlloc{}        -> arraysNotSupportedYet
  Neg e1            -> do
    (val, e1') <- evalExpr e1
    return (
        case val of
          IntV n -> IntV (-n)
          _      -> Dynamic
        ,
        Neg e1'
      )
  Not e1           -> do
    (val, e1') <- evalExpr e1
    return (
        case val of
          BooleanV b -> BooleanV (not b)
          _          -> Dynamic
        ,
        Not e1'
      )
  EBinOp e1 op e2 -> do
    (val1, e1') <- evalExpr e1
    (val2, e2') <- evalExpr e2
    let newOp = EBinOp e1' op e2'
    if (val1 /= Dynamic && val2 /= Dynamic) then
        (return (evalBinOp val1 op val2, newOp))
      else
        (return (               Dynamic, newOp))
  _                -> throwCheckError $ OtherException ("unexpected node " ++ show x)

getIntV :: Value -> Int
getIntV (IntV n) = n
getIntV _        = error "Invalid value type: expected int"

getStringV :: Value -> String
getStringV (StringV n) = n
getStringV _           = error "Invalid value type: expected string"

getBooleanV :: Value -> Bool
getBooleanV (BooleanV n) = n
getBooleanV _            = error "Invalid value type: expected bool"

evalBinOp :: Value -> Op -> Value -> Value
evalBinOp val1 op val2 = case op of
  Plus_Int -> opII_I (+)
  Minus    -> opII_I (-)
  Times    -> opII_I (*)
  Div      -> opII_I (div)
  Mod      -> opII_I (mod)
  LTH      -> opII_B (<)
  LE       -> opII_B (<=)
  GTH      -> opII_B (>)
  GE       -> opII_B (>=)
  EQU_Int  -> opII_B (==)
  NE_Int   -> opII_B (/=)
  AND      -> opBB_B (&&)
  OR       -> opBB_B (||)
  EQU_Bool -> opBB_B (==)
  NE_Bool  -> opBB_B (/=)
  Plus_Str ->  StringV  $ getStringV  val1 ++  getStringV  val2
  EQU_Str  ->  BooleanV $ getStringV  val1 ==  getStringV  val2
  NE_Str   ->  BooleanV $ getStringV  val1 /=  getStringV  val2
  _        -> error ("Unexpected operator " ++ show op)
  where
    opII_I f = IntV     $ getIntV     val1 `f` getIntV     val2
    opII_B f = BooleanV $ getIntV     val1 `f` getIntV     val2
    opBB_B f = BooleanV $ getBooleanV val1 `f` getBooleanV val2