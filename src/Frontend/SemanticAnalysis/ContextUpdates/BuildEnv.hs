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


buildEnv :: Program -> CheckM Env
buildEnv p = do
  venv <- use vEnv
  fenv <- getFunctions p
  cenv <- getClasses   p
  return (venv, fenv, cenv)

getClassFields :: [MemberDef] -> Env' Variable
getClassFields members =
  M.fromList $ do
    FieldDef (VarDef t items) <- members
    NoInit ident              <- items
    return (ident, Variable t ident)

getFunctionType :: FnDef -> Type
getFunctionType (FnDef rt _ args _) =
  FunT rt [ typ | (Arg typ _) <- args ]

args2vars :: [Arg] -> [Variable]
args2vars args = [ Variable typ ident | (Arg typ ident) <- args ]

getClassMethods :: [MemberDef] -> Env' Function
getClassMethods members =
  M.fromList $ do
    MetDef fd@(FnDef _ ident args body) <- members
    return (
        ident,
        Function (getFunctionType fd) ident (args2vars args) body
      )

updateSubclasses :: T.Tree Class -> T.Tree Class
updateSubclasses (T.Node super subs) =
  let
    newSubs = map (\x -> case x of
        T.Node (SubClass a _ b c) sc -> T.Node (SubClass a super b c) sc
        _                            -> error "Internal error"
      ) subs
  in
    T.Node super (map updateSubclasses newSubs)

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
      e2 = [ (SubClass name Object (getClassFields members) (getClassMethods members), name, [objectClassIdent]) |
                ClsTopDef (ClsDef   name         members) <- topdefs ]
      e3 = [ (SubClass name Object (getClassFields members) (getClassMethods members), name, [supName]) |
                ClsTopDef (ClsDefEx name supName members) <- topdefs ]
      (g, v2n, k2v) = G.graphFromEdges (e1 ++ e2 ++ e3)
      getCls v = let (c, _, _) = v2n v in c

getFunctions :: Program -> CheckM (Env' Function)
getFunctions (Program topdefs) =
  return.M.fromList $ do
    FnTopDef fd@(FnDef _ ident args body) <- topdefs
    return (
        ident,
        Function (getFunctionType fd) ident (args2vars args) body
      )