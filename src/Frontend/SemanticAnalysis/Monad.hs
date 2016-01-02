module Frontend.SemanticAnalysis.Monad where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Control.Lens

import Language.BuiltIns
import Frontend.SemanticAnalysis.CheckError




vEnv' :: Lens' Env (Env' Variable)
fEnv' :: Lens' Env (Env' Function)
cEnv' :: Lens' Env (Env'    Class)
vEnv' = lens (\(ve,  _,  _) -> ve) (\( _, fe, ce) ve -> (ve, fe, ce))
fEnv' = lens (\( _, fe,  _) -> fe) (\(ve,  _, ce) fe -> (ve, fe, ce))
cEnv' = lens (\( _,  _, ce) -> ce) (\(ve, fe,  _) ce -> (ve, fe, ce))

data CheckState
  = CheckState Env

env :: Lens' CheckState Env
env = lens (\(CheckState ev) -> ev) (const CheckState)

vEnv :: Lens' CheckState (Env' Variable)
fEnv :: Lens' CheckState (Env' Function)
cEnv :: Lens' CheckState (Env'    Class)
vEnv = env . vEnv'
fEnv = env . fEnv'
cEnv = env . cEnv'

initialState :: CheckState
initialState = CheckState (M.empty, M.empty, M.empty)

type CheckM' s a = StateT s (ExceptT CheckError IO) a
type CheckM  a   = CheckM' CheckState a

throwCheckError :: CEType -> CheckM' a b
throwCheckError et =
  throwError $ CheckError et []

throwTypeError :: TypeError -> CheckM' a b
throwTypeError et =
  throwCheckError $ TypeError et

infixl 0 $$
($$) :: CEContext -> CheckM' a b -> CheckM' a b
x $$ m = m `catchError` (\(CheckError et ctx) -> throwError $ CheckError et (x : ctx))