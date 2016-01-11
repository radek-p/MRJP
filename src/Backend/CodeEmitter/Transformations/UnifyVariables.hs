{-# LANGUAGE GADTs #-}
module Backend.CodeEmitter.Transformations.UnifyVariables where

import Prelude hiding ( id )
import Control.Monad.State
import Control.Monad.Identity
import Control.Lens hiding ( op, Empty )
import qualified Data.Map as M

import Frontend.Parser.AbsLatte


type IdxEnv = M.Map Ident Integer
type UnifyState = (IdxEnv, Integer)
type UnifyM a = StateT UnifyState Identity a


unifyVariables :: Program -> Program
unifyVariables x = runIdentity (evalStateT (unifyVariables' x) (M.empty, 0))

idxEnv :: Lens' UnifyState IdxEnv
idxEnv = lens (fst) (\(_, b) a -> (a, b))

nextIdx :: Lens' UnifyState Integer
nextIdx = lens (snd) (\(a, _) b -> (a, b))

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

item2ident :: Item -> Ident
item2ident (Init ident _) = ident
item2ident (NoInit ident) = ident

getUniqueIdent :: Ident -> UnifyM Ident
getUniqueIdent ident@(Ident str) = do
  idx <- uses idxEnv (M.!ident)
  return (Ident $ str ++ "_#" ++ show idx)

unifyVariables' :: Tree a -> UnifyM (Tree a)
unifyVariables' x = case x of
  FnDef _ _ args _ -> newScope $ do
    mapM_ declVar [ ident | Arg _ ident <- args ]
    composOpM unifyVariables' x
  Arg t ident -> do
    ident' <- getUniqueIdent ident
    return $ Arg t ident'
  BStmt _ -> newScope $ do
    composOpM unifyVariables' x
  Decl _ items -> do
    mapM_ declVar (map item2ident items)
    composOpM unifyVariables' x
  NoInit ident   -> do
    ident' <- getUniqueIdent ident
    return $ NoInit ident'
  Init   ident x -> do
    ident' <- getUniqueIdent ident
    return $ Init ident' x
  LVar ident -> do
    ident' <- getUniqueIdent ident
    return $ LVar ident'
  _ -> composOpM unifyVariables' x


