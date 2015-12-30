{-# LANGUAGE GADTs, KindSignatures, Rank2Types #-}
module Frontend.SemanticAnalysis.Checks.CyclicInherritance (checkCI) where

import Prelude hiding (cycle)
import qualified Data.Maybe as MB
import qualified Data.Graph as G
import qualified Data.Tree  as T

import Language.BuiltIns
import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.Monad
import Frontend.SemanticAnalysis.CheckError


data Class' = Class' Ident Ident

checkCI :: Program -> CheckM' a ()
checkCI (Program topdefs) = do
  let trees          = G.components g
  let topClassVertex = MB.fromJust $ k2v objectClassIdent
  case trees of
    []  -> throwCheckError (OtherException "Internal error")
    [_] -> return ()
    _   -> do
      let cycles  = [ lst   | lst <- map T.flatten trees, topClassVertex `notElem` lst ]
      let classes = [ [ ident | (_, ident, _) <- map v2n cycle ] | cycle <- cycles ]
      throwCheckError (CyclicInherritance classes)
    where
      e1 = [ (Class' objectClassIdent objectClassIdent, objectClassIdent, []) ]
      e2 = [ (Class' name objectClassIdent, name, [supName]) |
                ClsTopDef (ClsDefEx name supName _) <- topdefs ]
      e3 = [ (Class' name objectClassIdent, name, [objectClassIdent]) |
                ClsTopDef (ClsDef   name         _) <- topdefs ]
      (g, v2n, k2v) = G.graphFromEdges (e1 ++ e2 ++ e3)