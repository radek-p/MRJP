{-# LANGUAGE GADTs, KindSignatures, Rank2Types #-}
module Frontend.SemanticAnalysis.CheckError where

import Frontend.Parser.AbsLatte
import Frontend.Utility.PrettyPrinting


data CEType
  = OtherException String -- required by Error class
  | FunctionNamesNotUnique
  | ClassNamesNotUnique
  | MainFunctionNotDefined
  | UninitializedVarUsage Ident
  | CyclicInherritance [[Ident]]
  | ClassFieldInitialised
    deriving Show

data CEContext
  = forall a. CEContext (Tree a)

instance Show CEContext where
  show (CEContext x) = printTree x

data CheckError
  = CheckError CEType [CEContext]
    deriving Show