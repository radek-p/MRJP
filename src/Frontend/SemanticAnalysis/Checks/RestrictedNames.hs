{-# LANGUAGE GADTs, KindSignatures, Rank2Types #-}
module Frontend.SemanticAnalysis.Checks.CyclicInherritance (checkCI) where

import Control.Lens

import Language.BuiltIns
import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.Monad
import Frontend.SemanticAnalysis.CheckError


checkRN :: Program -> CheckM ()
checkRN (Program _) =
  checkRestrictedClassNames -- TODO Check other restricted names (such as this keyword)

checkRestrictedClassNames :: CheckM ()
checkRestrictedClassNames = do
  env <- use cEnv
  let restrictedUsedAsClass = [ ident | ident <- builtInIdentifiers, member ident env ]
  case restrictedUsed of
    h:_ -> throwCheckError $ RestrictedIdentifier h
    _   -> return ()