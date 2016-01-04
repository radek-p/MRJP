{-# LANGUAGE GADTs #-}
module Frontend.SemanticAnalysis.Transformations.UnifyOperators where

import Frontend.Parser.AbsLatte


unifyOperators :: Tree a -> Tree a
unifyOperators = composOp processNode

processNode :: Tree a -> Tree a
processNode x = unifyOperators $ case x of
  EMul e1 op e2 -> EBinOp e1 op e2
  EAdd e1 op e2 -> EBinOp e1 op e2
  ERel e1 op e2 -> EBinOp e1 op e2
  EAnd e1 op e2 -> EBinOp e1 op e2
  EOr  e1 op e2 -> EBinOp e1 op e2
  _             -> x
