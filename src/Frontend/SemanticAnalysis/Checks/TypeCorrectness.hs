{-# LANGUAGE GADTs, Rank2Types #-}
module Frontend.SemanticAnalysis.Checks.TypeCorrectness where

import Control.Lens
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
  ELitInt _       -> nc tInt
  ELitTrue        -> nc tBool
  ELitFalse       -> nc tBool
  EString _       -> nc tString

  ELitNull t      -> case t of
      SimpleT ident -> getClass ident >> nc t
      _             -> throwTypeError $ InvalidTypeOfNullLit t

  EApp ident args -> do
    funType <- getFunction ident
    let FunT resType expected = getType funType
    argTypes <- mapM checkType args
    let (actual, args') = unzip argTypes
    when (actual /= expected) $
      throwTypeError (TEApp actual expected)
    return (resType, EApp ident args')

  ELVal (LVar ident) -> getVariable ident >>= nc . getType

  ELVal (LArrAcc arr idx) -> do
    (tarr, arr') <- checkType arr
    (tidx, idx') <- checkType idx
    when (tidx /= tInt) $
      throwTypeError (TEArrIdx tidx)
    case tarr of
      ArrayT t -> return (t, ELVal $ LArrAcc arr' idx')
      _        -> throwTypeError (TEArr tarr)

  ELVal (LClsAcc obj fieldIdent) -> do
    (tobj, obj') <- checkType obj
    case tobj of
      SimpleT tident | not (isBuiltIn tident) -> do
        cls   <- getClass tident
        field <- getField fieldIdent cls
        return (getType field, ELVal (LClsAcc obj' fieldIdent))
      SimpleT{} | tobj == tString ->
        if fieldIdent == lengthIdent then
          return (tInt, ELVal (LClsAcc obj' fieldIdent))
        else
          throwTypeError $ FieldOfString fieldIdent
      SimpleT _ ->
        throwTypeError $ FieldOfBuiltIn tobj
      ArrayT _ ->
        if fieldIdent == lengthIdent then
          return (tInt, ELVal (LClsAcc obj' fieldIdent))
        else
          throwTypeError $ FieldOfArray fieldIdent
      FunT{} -> throwTypeError $ FieldOfFunction fieldIdent

  ClsApply _ _ _     -> error "TODO"
  ArrAlloc telem _   -> nc telem     --
  ClsAlloc typ       -> nc typ       -- TODO Check if class type

  Neg e1 -> do
    (typ, e1') <- checkType e1
    when (typ /= tInt) $
      throwTypeError (IncompatibleTypes tInt typ)
    return (tInt, e1')

  Not e1 -> do
    (typ, e1') <- checkType e1
    when (typ /= tBool) $
      throwTypeError (IncompatibleTypes tBool typ)
    return (tBool, e1')

  EBinOp _ _ _ -> error "TODO binops"

  _ -> error "Should not appear at this stage"

-- Helper functions

isSuperclassOf :: Class -> Class -> Bool
_  `isSuperclassOf` Object = False
c1 `isSuperclassOf` (SubClass _ super _ _) =
  c1 == super || c1 `isSuperclassOf` super

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