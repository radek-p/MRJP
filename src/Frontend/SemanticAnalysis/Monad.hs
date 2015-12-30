module Frontend.SemanticAnalysis.Monad where

import Control.Monad.Error
import Control.Monad.State

import Frontend.SemanticAnalysis.CheckError


type CheckMonad' s a = StateT s (ErrorT CheckError IO) a
type CheckMonad      = CheckMonad' () ()