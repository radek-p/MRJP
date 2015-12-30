module Frontend.SemanticAnalysis.Monad where

import Control.Monad.Except
import Control.Monad.State

import Frontend.SemanticAnalysis.CheckError


type CheckMonad' s a = StateT s (ExceptT CheckError IO) a
type CheckMonad      = CheckMonad' () ()

throwCheckError :: CEType -> CheckMonad' a b
throwCheckError et =
  throwError $ CheckError et []

infixl 0 $$
($$) :: CEContext -> CheckMonad' a b -> CheckMonad' a b
x $$ m = m `catchError` (\(CheckError et ctx) -> throwError $ CheckError et (x : ctx))