module Frontend.SemanticAnalysis.CheckError where

import Control.Monad.Error

import Frontend.Parser.AbsLatte


data CheckError
  = OtherException String -- required by Error class
  | FunctionNamesNotUnique
  | MainFunctionNotDefined
  | UninitializedVarUsage Ident
  | CyclicInherritance [[Ident]]
    deriving Show

instance Error CheckError where
  strMsg = OtherException
  noMsg  = strMsg ""