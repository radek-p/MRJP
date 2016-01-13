{-# LANGUAGE GADTs, Rank2Types #-}
module Frontend.SemanticAnalysis.Checks.TypeCorrectness where

import Control.Lens hiding ( op )
import Control.Monad
import Control.Monad.Except
import qualified Data.Map as M

import Frontend.SemanticAnalysis.Monad
import Frontend.SemanticAnalysis.CheckError
import Frontend.Parser.AbsLatte
import Language.BuiltIns


-- Check type correctness of program and replaces overloaded operators with
-- operations appropriate to the expected type.
checkTC :: Program -> CheckM (Program)
checkTC prog = checkStmt prog


-----------------------------------
-- Type analysis for statements  --
-----------------------------------

checkStmt :: Tree a -> CheckM (Tree a)
checkStmt = composOpM (\x -> observeStep x >> (CEContext x $$ checkStmtInner x))

newScope :: CheckM a -> CheckM a
newScope m = do
  outerEnv   <- use env
  outerScope <- use currentScope
  currentScope .= M.empty
  res <- m
  env          .= outerEnv
  currentScope .= outerScope
  return res

declVar :: Type -> Item -> CheckM Item
declVar typ x@(NoInit ident) = do
  scope <- use currentScope
  when (ident `M.member` scope) $
    throwTypeError (IdentifierAlreadyDefined ident)
  let doInsertion = M.insert ident $ Variable typ ident
  vEnv         %= doInsertion
  currentScope %= doInsertion
  return x

declVar typ (Init ident e1) = do
  scope <- use currentScope
  when (ident `M.member` scope) $
    throwTypeError (IdentifierAlreadyDefined ident)
  (type', e1') <- checkExpr e1
  type' <=! typ
  let doInsertion = M.insert ident $ Variable typ ident
  vEnv         %= doInsertion
  currentScope %= doInsertion
  return $ Init ident e1'

checkStmtInner :: Tree a -> CheckM (Tree a)
checkStmtInner x = case x of
  ClsDef{} -> objectsNotSupportedYet
  FnDef retType _ args _ -> newScope $ do
    mapM_ id $ [ declVar typ (NoInit ident) | Arg typ ident <- args]
    returnType .= retType
    checkStmt x
  Ret e1 -> do
    observeStep x
    (type', e1') <- checkExpr e1
    retType <- use returnType
    type' <=! retType
    return $ Ret e1'
  VRet -> do
    observeStep x
    retType <- use returnType
    retType <=! VoidT
    return VRet
  BStmt _  -> newScope $ checkStmt x
  Decl typ items -> do
    items' <- mapM (declVar typ) items
    return $ Decl typ items'
  Init ident e1 -> do
    var <- getVariable ident
    let type' = getType var
    (subtype', e1') <- checkExpr e1
    subtype' <=! type'
    return $ Init ident e1'
  Ass lval e1 -> do
    (type', ELVal lval') <- checkExpr (ELVal lval)
    (subtype, e1')       <- checkExpr e1
    subtype <=! type'
    return $ Ass lval' e1'
  Incr lval -> do
    (type', ELVal lval') <- checkExpr (ELVal lval)
    type' <=! IntT
    return $ Incr lval'
  Decr lval -> do
    (type', ELVal lval') <- checkExpr (ELVal lval)
    type' <=! IntT
    return $ Decr lval'
  Cond e1 s1 -> do
    (type', e1') <- checkExpr e1
    type' <=! BooleanT
    checkStmt (Cond e1' s1)
  CondElse e1 s1 s2 -> do
    (type', e1') <- checkExpr e1
    type' <=! BooleanT
    checkStmt (CondElse e1' s1 s2)
  While e1 s1 -> do
    (type', e1') <- checkExpr e1
    type' <=! BooleanT
    checkStmt (While e1' s1)
  For _ _ _ _ -> arraysNotSupportedYet
  SExp e1 -> do
    (_, e1') <- checkExpr e1
    return $ SExp e1'
  _  -> checkStmt x


-----------------------------------
-- Type analysis for expressions --
-----------------------------------

checkExpr :: Expr -> CheckM (Type, Expr)
checkExpr e1 = CEContext e1 $$  do
  (t, e) <- checkExprInner e1
  observeStep e1
  return (t, e)

checkExprInner :: Expr -> CheckM (Type, Expr)
checkExprInner e1@(ELitInt _) = return (    IntT, e1)
checkExprInner e1@(ELitTrue ) = return (BooleanT, e1)
checkExprInner e1@(ELitFalse) = return (BooleanT, e1)
checkExprInner e1@(EString _) = return ( StringT, e1)

checkExprInner (EApp ident args) = do
  funType  <- getFunction ident
  let FunT resType expected = getType funType
  argTypes <- mapM checkExpr args
  let (actual, args') = unzip argTypes
  when (length actual /= length expected) $
    throwTypeError (InvalidNumberOfArguments actual expected)
  zipWithM_ (<=!) actual expected
  return (resType, EApp ident args')

checkExprInner e1@(ELVal (LVar ident)) = do
  var <- getVariable ident
  return (getType var, e1)

-- Support for objects and arrays was not
-- finished yet so it was temporarily removed.
checkExprInner (ELitNull _         ) = objectsNotSupportedYet
checkExprInner (ELVal (LClsAcc _ _)) = objectsNotSupportedYet
checkExprInner (ClsApply _ _ _     ) = objectsNotSupportedYet
checkExprInner (ClsAlloc _         ) = objectsNotSupportedYet

checkExprInner (ArrAlloc _ _       ) = arraysNotSupportedYet
checkExprInner (ELVal (LArrAcc _ _)) = arraysNotSupportedYet

checkExprInner (Neg e1) = do
  (typ, e1') <- checkExpr e1
  typ <=! IntT
  return (IntT, Neg e1')

checkExprInner (Not e1) = do
  (typ, e1') <- checkExpr e1
  typ <=! BooleanT
  return (BooleanT, Not e1')

checkExprInner (EBinOp e1 op e2) = do
  (t1, e1') <- checkExpr e1
  (t2, e2') <- checkExpr e2
  let ensureEq t1' t2' ret = (t1 <=! t1') >> (t2 <=! t2') >> return (ret, EBinOp e1' op e2')
  let tII_I = ensureEq     IntT     IntT     IntT
  let tII_B = ensureEq     IntT     IntT BooleanT
  let tBB_B = ensureEq BooleanT BooleanT BooleanT
  case op of
    Minus -> tII_I
    Times -> tII_I
    Div   -> tII_I
    Mod   -> tII_I
    LTH   -> tII_B
    LE    -> tII_B
    GTH   -> tII_B
    GE    -> tII_B
    AND   -> tBB_B
    OR    -> tBB_B
    EQU   -> do
      when (t1 /= t2) $
        throwTypeError (IncompatibleTypes t1 t2)
      case t1 of
        IntT     -> return (BooleanT, EBinOp e1' EQU_Int  e2')
        BooleanT -> return (BooleanT, EBinOp e1' EQU_Bool e2')
        StringT  -> return (BooleanT, EBinOp e1' EQU_Str  e2')
        ArrayT _ -> arraysNotSupportedYet
        ClassT _ -> objectsNotSupportedYet
        _        -> throwTypeError $ InvalidOperandTypes t1 t2
    NE   -> do
      when (t1 /= t2) $
        throwTypeError (IncompatibleTypes t1 t2)
      case t1 of
        IntT     -> return (BooleanT, EBinOp e1' NE_Int  e2')
        BooleanT -> return (BooleanT, EBinOp e1' NE_Bool e2')
        StringT  -> return (BooleanT, EBinOp e1' NE_Str  e2')
        ArrayT _ -> arraysNotSupportedYet
        ClassT _ -> objectsNotSupportedYet
        _        -> throwTypeError $ InvalidOperandTypes t1 t2
    Plus  -> do
       when (t1 /= t2) $
         throwTypeError (IncompatibleTypes t1 t2)
       case t1 of
         IntT     -> return (IntT, EBinOp e1' Plus_Int e2')
         StringT  -> return (StringT, EBinOp e1' Plus_Str e2')
         _        -> throwTypeError $ InvalidOperandTypes t1 t2
    _     -> error "TODO Other?"

checkExprInner _ = throwCheckError $ OtherException "Pattern not matched"


-------------------------
-- Helper functions    --
-------------------------

isSuperclassOf :: Class -> Class -> Bool
_  `isSuperclassOf` Object = False
c1 `isSuperclassOf` (SubClass _ super _ _) =
  c1 == super || c1 `isSuperclassOf` super

compatible :: Type -> Type -> CheckM Bool
compatible subtype type' =
  return $ subtype == type'

ensureCompatible' :: Type -> Type -> TypeError -> CheckM ()
ensureCompatible' t1 t2 err = do
  res <- compatible t1 t2
  unless res (throwTypeError err)
  return ()

infix 0 <=!
(<=!) :: Type -> Type -> CheckM ()
(<=!) = ensureCompatible

infix 0 ~~!
(~~!) :: Type -> Type -> CheckM ()
t1 ~~! t2 = do
  (t1 <=! t2) `catchError` (const $ (t2 <=! t1))

ensureCompatible :: Type -> Type -> CheckM ()
ensureCompatible subtype type' = ensureCompatible' subtype type' (IncompatibleTypes subtype type')


