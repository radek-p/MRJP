module Frontend.SemanticAnalysis.Monad where

import Control.Monad.Except
import Control.Monad.State

import Frontend.SemanticAnalysis.CheckError


type CheckMonad' s a = StateT s (ExceptT CheckError IO) a
type CheckMonad      = CheckMonad' () ()