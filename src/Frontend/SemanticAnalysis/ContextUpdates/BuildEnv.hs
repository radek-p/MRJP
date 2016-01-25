{-# LANGUAGE GADTs, KindSignatures, Rank2Types #-}
module Frontend.SemanticAnalysis.ContextUpdates.BuildEnv where

import qualified Data.Map   as M
import qualified Data.Graph as G
import qualified Data.Tree  as T
import qualified Data.Maybe as MB
import Control.Lens

import Frontend.SemanticAnalysis.Monad
import Frontend.SemanticAnalysis.CheckError
import Language.BuiltIns
import Frontend.Parser.AbsLatte


buildEnv :: Program -> CheckM ()
buildEnv p = do
  fenv' <- getFunctions p
  cenv' <- getClasses   p
  fEnv %= (M.union fenv')
  cEnv %= (M.union cenv')

getClassFields :: [MemberDef] -> Env' ClassField
getClassFields members =
  M.fromList $ do
    (FieldDef t items, idx) <- zip members [0..]
    FdNoInit ident          <- items
    return (ident, ClassField (Variable t ident) idx)

getFunctionType :: FnDef -> Type
getFunctionType (FnDef rt _ args _) =
  FunT rt [ typ | (Arg typ _) <- args ]

--args2vars :: [Arg] -> [Variable]
--args2vars args = [ Variable typ ident | (Arg typ ident) <- args ]

getClassMethods :: [MemberDef] -> Env' Function
getClassMethods members =
  M.fromList $ do
    MetDef fd@(FnDef _ ident _args _body) <- members
    return (
        ident,
        Function (getFunctionType fd) ident
      )

updateSubclasses :: T.Tree Class -> T.Tree Class
updateSubclasses (T.Node super subs) =
  let
    newSubs = map (\x -> case x of
        T.Node (SubClass a _ b c d) sc -> T.Node (updateSizes $ SubClass a super b c d) sc
        _                              -> error "Internal error"
      ) subs
  in
    T.Node super (map updateSubclasses newSubs)

updateSizes :: Class -> Class
updateSizes Object = Object
updateSizes (SubClass _ident super _ _fEnv _mEnv) =
  let
    sizeDiff  = M.size _fEnv
    superSize = getSize super
    size      = superSize + sizeDiff
    _fEnv'    = fmap (\(ClassField v idx) -> (ClassField v $ idx + superSize)) _fEnv
  in
    (SubClass _ident super size _fEnv' _mEnv)

getClasses :: Program -> CheckM (Env' Class)
getClasses (Program topdefs) = do
  let trees          = G.components g
  let topClassVertex = MB.fromJust $ k2v objectClassIdent
  case trees of
    [_] -> do
      let [tree]  = G.dfs (G.transposeG g) [topClassVertex]
      let clsTree = fmap getCls tree
      return $ M.fromList [ (getIdent cls, cls) | cls <- T.flatten (updateSubclasses clsTree) ]
    _    ->
      throwCheckError (OtherException "Internal error - cyclic inherritance check failed")
    where
      e1 = [ (Object, objectClassIdent, []) ]
      e2 = [ (SubClass name Object 0 (getClassFields members) (getClassMethods members), name, [objectClassIdent]) |
                ClsTopDef (ClsDef   name         members) <- topdefs ]
      e3 = [ (SubClass name Object 0 (getClassFields members) (getClassMethods members), name, [supName]) |
                ClsTopDef (ClsDefEx name supName members) <- topdefs ]
      (g, v2n, k2v) = G.graphFromEdges (e1 ++ e2 ++ e3)
      getCls v = let (c, _, _) = v2n v in c

getFunctions :: Program -> CheckM (Env' Function)
getFunctions (Program topdefs) =
  return.M.fromList $ do
    FnTopDef fd@(FnDef _ ident _args _body) <- topdefs
    return (
        ident,
        Function (getFunctionType fd) ident
      )