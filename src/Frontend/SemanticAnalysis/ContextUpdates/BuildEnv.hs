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
  let
    fields  = [ (ident, t) | FieldDef t items <- members, FdNoInit ident   <- items ]
    fields' = [ (ident, ClassField (Variable t ident) idx) | ((ident, t), idx) <- zip fields [0..] ]
  in
    M.fromList fields'

getFunctionType :: FnDef -> Type
getFunctionType (FnDef rt _ args _) =
  FunT rt [ typ | (Arg typ _) <- args ]

--args2vars :: [Arg] -> [Variable]
--args2vars args = [ Variable typ ident | (Arg typ ident) <- args ]

getClassMethods :: Ident -> [MemberDef] -> Env' Method
getClassMethods clsident members =
  M.fromList $ do
    MetDef fd@(FnDef _ ident _args _body) <- members
    return (
        ident,
        Method (Function (getFunctionType fd) ident) (-1) clsident
      )

updateSubclasses :: T.Tree Class -> T.Tree Class
updateSubclasses (T.Node super subs) =
  let
    newSubs = map (\x -> case x of
        T.Node (SubClass a _ b c d) sc -> T.Node (updateMethodNumbers $ updateSizes $ SubClass a super b c d) sc
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

allMethods :: Class -> Env' Method
allMethods cls =
  let
    (newOrRedefined, inherrited) = allMethodsInner cls
  in
    M.union newOrRedefined inherrited

allMethodsInner :: Class -> (Env' Method, Env' Method)
allMethodsInner Object = (M.empty, M.empty)
allMethodsInner (SubClass _ super _ _ mE) =
  let
    superMethods      = allMethods super
    superMethodsCount = M.size superMethods
    newMethods        = M.elems $ M.difference mE superMethods
    inherritedMethods = M.difference superMethods mE
    redefinedMethods  = M.intersectionWith (\(Method f _ ci) (Method _ idx _) -> Method f idx ci) mE superMethods
    newMethods'       = M.fromList [ (getIdent f, Method f (superMethodsCount + num) ci) | (Method f _ ci, num) <- zip newMethods [0..]]
  in
    (M.union newMethods' redefinedMethods, inherritedMethods)

updateMethodNumbers :: Class -> Class
updateMethodNumbers Object = Object
updateMethodNumbers cls@(SubClass a b c d mE2) =
  let
    (newOrRedefined, _) = allMethodsInner cls
  in
    SubClass a b c d $ M.union newOrRedefined mE2

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
      e2 = [ (SubClass name Object 0 (getClassFields members) (getClassMethods name members), name, [objectClassIdent]) |
                ClsTopDef (ClsDef   name         members) <- topdefs ]
      e3 = [ (SubClass name Object 0 (getClassFields members) (getClassMethods name members), name, [supName]) |
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