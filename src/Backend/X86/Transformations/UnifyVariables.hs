{-# LANGUAGE GADTs #-}
module Backend.X86.Transformations.UnifyVariables where

import Prelude hiding ( id )
import Control.Monad.State
import Control.Lens hiding ( op, Empty )
import qualified Data.Map as M

import Language.BuiltIns
import Frontend.Parser.AbsLatte


-------------------------------------------------------
-- Make names of variables unique by appending an id --
-------------------------------------------------------

type IdxEnv = M.Map Ident Integer
type UnifyState = (IdxEnv, Integer, Bool)
type UnifyM a = StateT UnifyState IO a

unifyVariables :: Program -> IO Program
unifyVariables x = evalStateT (unifyVariables' x) (M.empty, 0, False)

idxEnv :: Lens' UnifyState IdxEnv
idxEnv = _1

nextIdx :: Lens' UnifyState Integer
nextIdx = _2

inClass :: Lens' UnifyState Bool
inClass = _3

newScope :: UnifyM a -> UnifyM a
newScope m = do
  outerEnv <- use idxEnv
  res      <- m
  idxEnv   .= outerEnv
  return res

declVar :: Ident -> UnifyM ()
declVar ident = do
  idx <- use nextIdx
  nextIdx %= (+1)
  idxEnv  %= (M.insert ident idx)

processItem :: Item -> UnifyM Item
processItem (Init ident e1) = do
  e1' <- unifyVariables' e1
  declVar ident
  return (Init ident e1')
processItem x@(NoInit ident) = do
  declVar ident
  return x

getUniqueIdent :: Ident -> UnifyM Ident
getUniqueIdent ident@(Ident str) = do
  idx <- uses idxEnv (\oenv -> M.findWithDefault (error $ "gu not found " ++ show ident ++ " in " ++ show oenv) ident oenv)
  return (Ident $ str ++ "_#" ++ show idx)

unifyVariables' :: Tree a -> UnifyM (Tree a)
unifyVariables' x = case x of
  ClsDefEx{} -> do
    inClass .= True
    res <- composOpM unifyVariables' x
    inClass .= False
    return res
  ClsDef{} -> do
    inClass .= True
    res <- composOpM unifyVariables' x
    inClass .= False
    return res
  FnDef _ _ args _ -> newScope $ do
    q <- use inClass
    let args' = if q then [thisIdent] else [] ++ [ ident | Arg _ ident <- args ]
    mapM_ declVar args'
    composOpM unifyVariables' x
  Arg t ident -> do
    ident' <- getUniqueIdent ident
    return $ Arg t ident'
  BStmt _ -> newScope $ do
    composOpM unifyVariables' x
  Decl q items -> do
    items' <- mapM processItem items
    composOpM unifyVariables' (Decl q items')
  NoInit ident   -> do
    ident' <- getUniqueIdent ident
    return $ NoInit ident'
  Init   ident e1 -> do
    ident' <- getUniqueIdent ident
    return $ Init ident' e1 -- e1 was already processed in Decl
  LVar ident -> do
    ident' <- getUniqueIdent ident
    return $ LVar ident'
  _ -> composOpM unifyVariables' x


