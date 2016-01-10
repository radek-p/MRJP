{-# LANGUAGE GADTs #-}
module Frontend.SemanticAnalysis.Transformations.SimpleElimination where

import Prelude hiding ( id )

import Frontend.SemanticAnalysis.Monad
import Frontend.Parser.AbsLatte


simpleElimination :: Tree a -> CheckM (Tree a)
simpleElimination = composOpM (simpleEliminationInner)

simpleEliminationInner :: Tree a -> CheckM (Tree a)
simpleEliminationInner x = case x of
  Cond     (ELitTrue ) s1   -> simpleElimination s1
  Cond     (ELitFalse) _    -> return Empty
  CondElse (ELitTrue ) s1 _ -> simpleElimination s1
  CondElse (ELitFalse) _ s2 -> simpleElimination s2
  While    (ELitFalse) _    -> return Empty
  _                         -> simpleElimination x