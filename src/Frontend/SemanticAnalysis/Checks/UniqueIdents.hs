{-# LANGUAGE GADTs, KindSignatures, Rank2Types, DataKinds, PolyKinds, FlexibleContexts #-}
module Frontend.SemanticAnalysis.Checks.UniqueIdents (checkIdentsUnique) where

import Prelude hiding (cycle)
import Control.Monad
import Control.Monad.State
import Control.Lens
import qualified Data.Set as S
import qualified Data.Map as M

import Language.BuiltIns
import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.Monad
import Frontend.SemanticAnalysis.CheckError


checkIdentsUnique :: Program -> CheckM ()
checkIdentsUnique p = checkFunctionNames p >> checkClassNames p

checkFunctionNames :: Program -> CheckM ()
checkFunctionNames (Program topdefs)
  | length idents /= S.size idents'       = throwCheckError FunctionNamesNotUnique
  | not $ S.member (Ident "main") idents' = throwCheckError MainFunctionNotDefined
  | otherwise                             = do
      fenv <- use fEnv
      let builtIns = map getIdent $ M.elems fenv
      unless (S.null (S.fromList builtIns `S.intersection` idents')) $
        throwCheckError (RedefinitionOfBuiltInFunctions builtIns)
      liftIO $ print idents'
    where
      idents  = [ ident | FnTopDef (FnDef _ ident _ _) <- topdefs ]
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