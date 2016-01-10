{-# LANGUAGE GADTs #-}
module Frontend.SemanticAnalysis.Checks.ProperReturnStatements where

import Data.Monoid
import Control.Monad

import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.Monad
import Frontend.SemanticAnalysis.CheckError


checkRS :: Program -> CheckM ()
checkRS = checkRS'

checkRS' :: Tree a -> CheckM ()
checkRS' = composOpM_ (\x -> CEContext x $$ checkRS'Inner x)

checkRS'Inner :: Tree a -> CheckM ()
checkRS'Inner x = case x of
  FnDef type' ident _ bl -> unless (type' == VoidT || getAny (hasReturn bl)) $
    throwCheckError (MissingReturnStatement ident)
  _ -> checkRS' x

hasReturn :: Tree a -> Any
hasReturn x = case x of
  Ret {}           -> Any True
  VRet{}           -> Any True
  Cond{}           -> Any False
  CondElse _ s1 s2 -> Any $ getAny (hasReturn s1) && getAny (hasReturn s2)
  _                -> composOpMonoid hasReturn $ x