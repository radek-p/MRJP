{-# LANGUAGE GADTs, Rank2Types #-}
module Frontend.SemanticAnalysis.Checks.TypeCorrectness where

import Control.Lens hiding ( op )
import Control.Monad
import qualified Data.Map as M

import Frontend.SemanticAnalysis.Monad
import Frontend.SemanticAnalysis.CheckError
import Frontend.Parser.AbsLatte
import Language.BuiltIns


-- Check type correctness of program and replaces overloaded operators with
-- operations appropriate to the expected type.
checkTC :: Program -> CheckM (Program)
checkTC prog = checkTC' prog

checkTC' :: forall a. Tree a -> CheckM (Tree a)
checkTC' x = case x of
  _ -> return x


checkType :: Expr -> CheckM (Type, Expr)
checkType e = CEContext e $$ let nc typ = return (typ, e) in case e of
  ELitInt _       -> nc IntT
  ELitTrue        -> nc BooleanT
  ELitFalse       -> nc BooleanT
  EString _       -> nc StringT

  ELitNull t      -> case t of
      ClassT ident -> getClass ident >> nc t
      ArrayT _     -> error "TODO nulls as arrays"
      _            -> throwTypeError $ InvalidTypeOfNullLit t

  EApp ident args -> do
    funType <- getFunction ident
    let FunT resType expected = getType funType
    argTypes <- mapM checkType args
    let (actual, args') = unzip argTypes
    zipWithM_ ensureCompatible actual expected
    return (resType, EApp ident args')

  ELVal (LVar ident) -> getVariable ident >>= nc . getType

  ELVal (LArrAcc arr idx) -> do
    (tarr, arr') <- checkType arr
    (tidx, idx') <- checkType idx
    ensureCompatible' tidx IntT $ TEArrIdx tidx
    case tarr of
      ArrayT t -> return (t, ELVal $ LArrAcc arr' idx')
      _        -> throwTypeError (TEArr tarr)

  ELVal (LClsAcc obj fieldIdent) -> do
    (tobj, obj') <- checkType obj
    case tobj of
      ClassT tident -> do
        cls   <- getClass tident
        field <- getField fieldIdent cls
        return (getType field, ELVal (LClsAcc obj' fieldIdent))
      StringT ->
        if fieldIdent == lengthIdent then
          return (IntT, ELVal (LClsAcc obj' fieldIdent))
        else
          throwTypeError $ FieldOfString fieldIdent
      VoidT    -> throwTypeError $ FieldOfBuiltIn tobj
      IntT     -> throwTypeError $ FieldOfBuiltIn tobj
      BooleanT -> throwTypeError $ FieldOfBuiltIn tobj
      ArrayT _ ->
        if fieldIdent == lengthIdent then
          return (IntT, ELVal (LClsAcc obj' fieldIdent))
        else
          throwTypeError $ FieldOfArray fieldIdent
      FunT{} -> throwTypeError $ FieldOfFunction fieldIdent

  ClsApply _ _ _     -> error "TODO"

  ArrAlloc telem e1  -> do
    (typ, e1') <- checkType e1
    ensureCompatible' typ IntT $ TEArrAlocLen typ
    return (ArrayT telem, ArrAlloc telem e1')

  ClsAlloc typ       -> case typ of
    ClassT clsIdent -> nc typ
    _               -> throwTypeError $ ObjAllocBadType typ

  Neg e1 -> do
    (typ, e1') <- checkType e1
    ensureCompatible typ IntT
    return (IntT, e1')

  Not e1 -> do
    (typ, e1') <- checkType e1
    ensureCompatible typ BooleanT
    return (BooleanT, e1')

  EBinOp e1 op e2 -> do
    (t1, e1') <- checkType e1
    (t2, e2') <- checkType e2
    let ensureEq t1' t2' ret = ensureCompatible t1 t1' >> ensureCompatible t2 t2' >> return (ret, EBinOp e1' op e2')
    let tII_I = ensureEq IntT  IntT  IntT
    let tII_B = ensureEq IntT  IntT  BooleanT
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
      EQU   -> error "TODO =="
      NE    -> error "TODO !="
      Plus  -> error "TODO Plus"
      _     -> error "TODO Other?"


  _ -> error "Should not appear at this stage"

-- Helper functions

isSuperclassOf :: Class -> Class -> Bool
_  `isSuperclassOf` Object = False
c1 `isSuperclassOf` (SubClass _ super _ _) =
  c1 == super || c1 `isSuperclassOf` super

compatible :: Type -> Type -> CheckM Bool
compatible t1 t2 = return $ t1 == t2 -- TODO inherritance

ensureCompatible' :: Type -> Type -> TypeError -> CheckM ()
ensureCompatible' t1 t2 err = do
  res <- compatible t1 t2
  unless res (throwTypeError err)
  return ()

ensureCompatible :: Type -> Type -> CheckM ()
ensureCompatible t1 t2 = ensureCompatible' t1 t2 (IncompatibleTypes t1 t2)

getVariable :: Ident -> CheckM Variable
getVariable ident = do
  e <- use vEnv
  case M.lookup ident e of
    Just var -> return var
    Nothing  -> throwTypeError $ VariableNotFound ident

getClass :: Ident -> CheckM Class
getClass ident = do
  e <- use cEnv
  case M.lookup ident e of
    Just cls -> return cls
    Nothing  -> throwTypeError $ ClassNotFound ident

getClassItem :: ((Env' Variable, Env' Function) -> Env' c) -> (Ident -> Class -> TypeError) -> Ident -> Class -> Class -> CheckM c
getClassItem _ err ident orig (Object) =
  throwTypeError $ err ident orig

getClassItem component err ident orig (SubClass _ super fiEnv mEnv) =
  case M.lookup ident (component (fiEnv, mEnv)) of
    Just method -> return method
    Nothing     -> getClassItem component err ident orig super

getField :: Ident -> Class -> CheckM Variable
getField ident cls = getClassItem fst FieldNotFound ident cls cls

getMethod :: Ident -> Class -> CheckM Function
getMethod ident cls = getClassItem snd MethodNotFound ident cls cls

getFunction :: Ident -> CheckM Function
getFunction ident = do
  e <- use fEnv
  case M.lookup ident e of
    Just fun -> return fun
    Nothing  -> throwTypeError $ FunctionNotFound ident