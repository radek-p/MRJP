{-# LANGUAGE GADTs, KindSignatures, Rank2Types #-}
module Frontend.SemanticAnalysis.Checks.NoNestedDecls where

import Control.Monad

import Frontend.Parser.ComposOp
import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.Monad
import Frontend.SemanticAnalysis.CheckError


checkNND :: Program -> CheckM ()
checkNND = checkNND' False

checkNND' :: Bool -> Tree a -> CheckM ()
checkNND' allow = composOpM_ (checkNode allow)

checkNode :: forall a. Bool -> Tree a -> CheckM ()
checkNode allow x = CEContext x $$ do
  case x of
    Block _ -> checkNND' True x
    Decl{}  -> unless allow $ throwCheckError NestedVariableDeclaration
    _       -> checkNND' False x

