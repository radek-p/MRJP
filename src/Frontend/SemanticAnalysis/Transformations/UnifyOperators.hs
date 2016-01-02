{-# LANGUAGE GADTs #-}
module Frontend.SemanticAnalysis.Transformations.UnifyOperators where

import Frontend.Parser.AbsLatte


unifyOperators :: Tree a -> Tree a
unifyOperators = composOp processNode

processNode :: Tree a -> Tree a
processNode x = case x of
  EMul e1 op e2 -> mkOp e1 op e2
  EAdd e1 op e2 -> mkOp e1 op e2
  ERel e1 op e2 -> mkOp e1 op e2
  EAnd e1 op e2 -> mkOp e1 op e2
  EOr  e1 op e2 -> mkOp e1 op e2
  _             -> unifyOperators x
  where
    mkOp e1 op e2 =
      EBinOp (unifyOperators e1) op (unifyOperators e2)
