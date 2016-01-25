{-# LANGUAGE GADTs, KindSignatures, Rank2Types, DataKinds, PolyKinds, FlexibleContexts #-}
module Frontend.SemanticAnalysis.Checks.UniqueIdents (checkIdentsUnique) where

import Prelude hiding (cycle)
import Control.Monad
import Control.Lens
import qualified Data.Set as S
import qualified Data.Map as M

import Language.BuiltIns
import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.Monad
import Frontend.SemanticAnalysis.CheckError


checkIdentsUnique :: Program -> CheckM ()
checkIdentsUnique p = do
  mapM_ (\a -> a p) [
      checkFunctionNames, checkClassNames, checkClasses
    ]

checkFunctionNames :: Program -> CheckM ()
checkFunctionNames (Program topdefs)
  | length idents /= S.size idents'       = throwCheckError FunctionNamesNotUnique
  | not $ S.member (Ident "main") idents' = throwCheckError MainFunctionNotDefined
  | otherwise                             = do
      fenv <- use fEnv
      let builtIns = map getIdent $ M.elems fenv
      let intersection = S.fromList builtIns `S.intersection` idents'
      unless (S.null intersection) $
        throwCheckError (RedefinitionOfBuiltInFunctions $ S.toList intersection)
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

checkClasses :: Program -> CheckM ()
checkClasses (Program topdefs) =
  mapM_ checkClass classes
  where
    classes1 = [ (ident, memdefs) | ClsTopDef (ClsDef   ident   memdefs) <- topdefs ]
    classes2 = [ (ident, memdefs) | ClsTopDef (ClsDefEx ident _ memdefs) <- topdefs ]
    classes = classes1 ++ classes2

checkClass :: (Ident, [MemberDef]) -> CheckM ()
checkClass (cident, memdefs)
  | length fields1  /= S.size fields  = throwCheckError $ ClassFieldsNotUnique  cident
  | length methods1 /= S.size methods = throwCheckError $ ClassMethodsNotUnique cident
  | thisIdent `S.member` fields       = throwCheckError $ ThisField             cident
  | otherwise                         = return ()
  where
    fields1  = [ ident | FieldDef _ items           <- memdefs, FdNoInit ident <- items ]
    methods1 = [ ident | MetDef (FnDef _ ident _ _) <- memdefs ]
    fields   = S.fromList $ fields1
    methods  = S.fromList $ methods1