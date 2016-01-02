{-# LANGUAGE GADTs, KindSignatures, Rank2Types #-}
module Frontend.SemanticAnalysis.Checks.FieldsNotInitialised (checkFNI) where

import Frontend.Parser.ComposOp
import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.Monad
import Frontend.SemanticAnalysis.CheckError
import Control.Monad


checkFNI :: Tree a -> CheckM ()
checkFNI = composOpM_ checkFieldDefs

checkFieldDefs :: forall a. Tree a -> CheckM ()
checkFieldDefs x = CEContext x $$ do
  case x of
    FieldDef vd -> checkNotInitialised vd
    _           -> checkFNI x

checkNotInitialised :: VarDef -> CheckM ()
checkNotInitialised vd@(VarDef _ vs) = (CEContext vd) $$ do
  (when (not (null [ () | Init _ _ <- vs]))
    $ throwCheckError (ClassFieldInitialised))

