{-# LANGUAGE GADTs, KindSignatures, Rank2Types, DataKinds, PolyKinds, FlexibleContexts #-}
module Frontend.SemanticAnalysis.CheckError where

--import Control.Monad.Error

import Frontend.Parser.AbsLatte
import Frontend.Utility.PrettyPrinting


data CEType
  = OtherException String -- required by Error class
  | FunctionNamesNotUnique
  | MainFunctionNotDefined
  | UninitializedVarUsage Ident
  | CyclicInherritance [[Ident]]
    deriving Show

data CEContext
  = forall a. CEContext (Tree a)

instance Show CEContext where
  show (CEContext x) = printTree x

data CheckError
  = CheckError CEType [CEContext]
    deriving Show