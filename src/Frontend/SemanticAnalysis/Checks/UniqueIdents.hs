{-# LANGUAGE GADTs, KindSignatures, Rank2Types, DataKinds, PolyKinds, FlexibleContexts #-}
module Frontend.SemanticAnalysis.Checks.UniqueIdents (checkIdentsUnique) where

import Prelude hiding (cycle)
import qualified Data.Set   as S

import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.Monad
import Frontend.SemanticAnalysis.CheckError


checkIdentsUnique :: Program -> CheckM ()
checkIdentsUnique = checkFunctionNames >> checkClassNames

checkFunctionNames :: Program -> CheckM ()
checkFunctionNames (Program topdefs)
  | length idents /= S.size idents' = throwCheckError FunctionNamesNotUnique
  | not $ S.member "main" idents'   = throwCheckError MainFunctionNotDefined
  | otherwise                       = return ()
    where
      idents  = [ ident | FnTopDef (FnDef _ (Ident ident) _ _) <- topdefs ]
      idents' = S.fromList idents

checkClassNames :: Program -> CheckM ()
checkClassNames (Program topdefs)
  | length idents /= S.size idents' = throwCheckError ClassNamesNotUnique
  | otherwise                       = return ()
    where
      idents1 = [ ident | ClsTopDef (ClsDef   ident   _) <- topdefs ]
      idents2 = [ ident | ClsTopDef (ClsDefEx ident _ _) <- topdefs ]
      idents  = idents1 ++ idents2
      idents' = S.fromList idents