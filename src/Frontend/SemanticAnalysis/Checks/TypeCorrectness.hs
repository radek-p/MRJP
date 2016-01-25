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

forbidVoid :: Type -> CheckM ()
forbidVoid t = do
  when (t == VoidT) $ throwTypeError VoidNotAllowed
  case t of
    ArrayT inner -> forbidVoid inner
    _            -> return ()

withClass :: Ident -> CheckM a -> CheckM a
withClass ident a = do
 cls <- getClass ident
 currentClass .= Just cls
 ret <- a
 currentClass .= Nothing
 return ret

declVar :: Type -> Item -> CheckM Item
declVar typ x@(NoInit ident) = do
  forbidVoid typ
  scope <- use currentScope
  when (ident `M.member` scope) $
    throwTypeError (IdentifierAlreadyDefined ident)
  let doInsertion = M.insert ident $ Variable typ ident
  vEnv         %= doInsertion
  currentScope %= doInsertion
  return x

declVar typ (Init ident e1) = do
  forbidVoid typ
  scope <- use currentScope
  when (ident `M.member` scope) $
    throwTypeError (IdentifierAlreadyDefined ident)
  (type', e1') <- checkExpr e1                                     -- <---------------???
  type' <=! typ
  let doInsertion = M.insert ident $ Variable typ ident
  vEnv         %= doInsertion
  currentScope %= doInsertion
  return $ Init ident e1'

checkStmtInner :: Tree a -> CheckM (Tree a)
checkStmtInner x = case x of
  ClsDefEx ident _ _ -> withClass ident $ checkStmt x
  ClsDef   ident   _ -> withClass ident $ checkStmt x
  MetDef   (FnDef retType _ args _) -> newScope $ do
    sequence_ [ declVar typ (NoInit ident) | Arg typ ident <- args]
    mcls <- use currentClass
    _ <- case mcls of
      Just cls -> declVar (getType cls) (NoInit thisIdent)
      Nothing  -> error "Internal error: class not specified"
    returnType .= retType
    checkStmt x
  FnTopDef (FnDef retType fname args _) -> newScope $ do
    when (fname == Ident "main" && (not.null) args) $
      throwTypeError (InvalidMainSignature args)
    sequence_ [ declVar typ (NoInit ident) | Arg typ ident <- args]
    returnType .= retType
    checkStmt x
  Ret e1 -> do
    (t1', e1')       <- checkExpr e1
    retType          <- use returnType
    forbidVoid t1'
    t1' <=! retType
    return $ Ret e1'
  VRet -> do
    retType          <- use returnType
    retType ==! VoidT
    return VRet
  BStmt _  ->
    newScope $ checkStmt x
  Decl typ items -> do
    items'           <- mapM (declVar typ) items
    return $ Decl typ items'
  Init ident1 e2 -> do
    v1               <- getVariable ident1                         -- <---------------??? To samo?
    (t2', e2')       <- checkExpr e2
    t2' <=! getType v1
    return $ Init ident1 e2'
  Ass l1 e2 -> do
    (t1', ELVal l1') <- checkExpr (ELVal l1)
    (t2',       e2') <- checkExpr e2
    case l1' of -- enure that there are no assignments to array's length field
      LTClsAcc t3 _ _ -> when (isArrayType t3) $
        throwTypeError LengthIsNotWritable
      _               -> return ()
    t2' <=! t1'
    return $ Ass l1' e2'
  Incr l1 -> do
    (t1', ELVal l1') <- checkExpr (ELVal l1)
    t1' ==! IntT
    return $ Incr l1'
  Decr l1 -> do
    (t1', ELVal l1') <- checkExpr (ELVal l1)
    t1' ==! IntT
    return $ Decr l1'
  Cond e1 s1 -> do
    (t1', e1')       <- checkExpr e1
    t1' ==! BooleanT
    checkStmt (Cond e1' s1)
  CondElse e1 s2 s3 -> do
    (t1', e1')     <- checkExpr e1
    t1' <=! BooleanT
    checkStmt (CondElse e1' s2 s3)
  While e1 s2 -> do
    (t1', e1')     <- checkExpr e1
    t1' ==! BooleanT
    checkStmt (While e1' s2)
  For t1 ident e3 s4 -> newScope $ do
    (t3', e3') <- checkExpr e3
    case t3' of
      ArrayT t3'elem -> do
        t3'elem <=! t1
        _ <- declVar t1 (NoInit ident)
        return ()
      _              ->
        throwCheckError (OtherException "TODO Exception")
    checkStmt (For t1 ident e3' s4)
  SExp e1 -> do
    (_, e1') <- checkExpr e1
    return $ SExp e1'
  _  -> checkStmt x


-----------------------------------
-- Type analysis for expressions --
-----------------------------------

checkApp :: Function -> [Expr] -> CheckM (Type, [Expr])
checkApp fun args = do
  let FunT resType expected = getType fun
  argTypes <- mapM checkExpr args
  let (actual, args') = unzip argTypes
  when (length actual /= length expected) $
    throwTypeError (InvalidNumberOfArguments actual expected)
  zipWithM_ (<=!) actual expected
  return (resType, args')

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
  mcls <- use currentClass

  methodExists <- case mcls of
    Just cls -> catchError (getMethod ident cls >> return True) (const $ return False)
    _        -> return False

  case methodExists of
    True  -> checkExpr $ ClsApply (ELVal $ LVar thisIdent) ident args
    False -> do -- if we failed to find apropriate method we have to search fEnv also
      fun              <- getFunction ident
      (retType, args') <- checkApp fun args
      return (retType, EApp ident args')

checkExprInner e1@(ELVal (LVar ident)) = catchError (do
    var <- getVariable ident
    return (getType var, e1)
  ) (\err -> do
    mcls <- use currentClass
    case mcls of -- If we're checking a method of class, we also have to inspect its fields
      Just cls -> do
        field <- getField ident cls
        return (getType field, ELVal (LClsAcc (ELVal $ LVar thisIdent) ident))
      _        -> throwError err
  )

checkExprInner e1@(ELitNull t1        ) = do
  unless (isArrayType t1 || isObjectType t1) $
    throwCheckError (OtherException "TODO Type has no null value")
  return (t1, e1)

checkExprInner (ELVal (LClsAcc e1 i2)) = do
  (t1', e1') <- checkExpr e1
  case t1' of
    ArrayT _     -> do
      when (i2 /= lengthIdent) $
        throwTypeError (FieldOfArray i2)
      return (IntT, (ELVal (LTClsAcc t1' e1' i2)))
    ClassT clsid -> do
      cls   <- getClass clsid
      field <- getField i2 cls
      return (getType field, (ELVal (LTClsAcc t1' e1' i2)))
    _               -> throwTypeError (FieldOfBuiltIn t1' i2)

checkExprInner (ClsApply e1 i2 args) = do
  (t1', e1') <- checkExpr e1
  case t1' of
    ClassT clsid -> do
      cls         <- getClass clsid
      method      <- getMethod i2 cls
      (rt, args') <- checkApp method args
      return (rt, TClsApply (getType cls) e1' i2 args')
    _            -> throwCheckError (OtherException "TODO invalid class apply")

checkExprInner x@(ClsAlloc clsid   ) = do
  cls <- getClass clsid
  return (getType cls, x)

checkExprInner (ArrAlloc t1 e2     ) = do
  (t2', e2') <- checkExpr e2
  t2' ==! IntT
  return (ArrayT t1, ArrAlloc t1 e2')

checkExprInner (ELVal (LArrAcc e1 e2)) = do
  (t1', e1') <- checkExpr e1
  (t2', e2') <- checkExpr e2
  t2' ==! IntT
  case t1' of
    ArrayT t1'elem -> return (t1'elem, ELVal (LArrAcc e1' e2'))
    _              -> throwCheckError (OtherException "TODO Invvalid array access")

checkExprInner (Neg e1) = do
  (typ, e1') <- checkExpr e1
  typ ==! IntT
  return (IntT, Neg e1')

checkExprInner (Not e1) = do
  (typ, e1') <- checkExpr e1
  typ ==! BooleanT
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
        throwTypeError (IncompatibleTypes t2 t1)
      case t1 of
        IntT     -> return (BooleanT, EBinOp e1' EQU_Int  e2')
        BooleanT -> return (BooleanT, EBinOp e1' EQU_Bool e2')
        StringT  -> return (BooleanT, EBinOp e1' EQU_Str  e2')
        ArrayT _ -> return (BooleanT, EBinOp e1' EQU_Arr  e2')
        ClassT _ -> return (BooleanT, EBinOp e1' EQU_Ref  e2')
        _        -> throwTypeError $ InvalidOperandTypes t1 t2
    NE   -> do
      when (t1 /= t2) $
        throwTypeError (IncompatibleTypes t2 t1)
      case t1 of
        IntT     -> return (BooleanT, EBinOp e1' NE_Int  e2')
        BooleanT -> return (BooleanT, EBinOp e1' NE_Bool e2')
        StringT  -> return (BooleanT, EBinOp e1' NE_Str  e2')
        ArrayT _ -> return (BooleanT, EBinOp e1' NE_Arr  e2')
        ClassT _ -> return (BooleanT, EBinOp e1' NE_Ref  e2')
        _        -> throwTypeError $ InvalidOperandTypes t1 t2
    Plus  -> do
       when (t1 /= t2) $
         throwTypeError (IncompatibleTypes t2 t1)
       case t1 of
         IntT     -> return (IntT, EBinOp e1' Plus_Int e2')
         StringT  -> return (StringT, EBinOp e1' Plus_Str e2')
         _        -> throwTypeError $ InvalidOperandTypes t1 t2
    _     -> error "TODO Other?"

checkExprInner _ = throwCheckError $ OtherException "Pattern not matched"


-------------------------
-- Helper functions    --
-------------------------

ensureEqual :: Type -> Type -> CheckM ()
ensureEqual t1 t2 =
  unless (t1 == t2) $
    throwCheckError (OtherException $ "TODO Types should be equal: " ++ show t1 ++ " " ++ show t2)


isSuperclassOf :: Class -> Class -> Bool
_  `isSuperclassOf` Object = False
c1 `isSuperclassOf` (SubClass _ super _ _ _) =
  c1 == super || c1 `isSuperclassOf` super

compatible :: Type -> Type -> CheckM Bool
compatible (ArrayT subtype) (ArrayT type') =
  compatible subtype type'

compatible (ClassT subclassId) (ClassT superclassId) = do
  subclass   <- getClass subclassId
  superclass <- getClass superclassId
  return (superclass == subclass || superclass `isSuperclassOf` subclass)

compatible subtype type' =
  return $ subtype == type'

ensureCompatible' :: Type -> Type -> TypeError -> CheckM ()
ensureCompatible' t1 t2 err = do
  res <- compatible t1 t2
  unless res (throwTypeError err)
  return ()

ensureCompatible :: Type -> Type -> CheckM ()
ensureCompatible subtype type' = ensureCompatible' subtype type' (IncompatibleTypes subtype type')


infix 0 ==!
(==!) :: Type -> Type -> CheckM ()
(==!) = ensureEqual

infix 0 <=!
(<=!) :: Type -> Type -> CheckM ()
(<=!) = ensureCompatible

infix 0 ~~!
(~~!) :: Type -> Type -> CheckM ()
t1 ~~! t2 = do
  (t1 <=! t2) `catchError` (const $ (t2 <=! t1))



