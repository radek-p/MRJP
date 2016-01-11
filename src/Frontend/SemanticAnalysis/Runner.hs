{-# LANGUAGE GADTs, KindSignatures, Rank2Types, DataKinds, PolyKinds, FlexibleContexts #-}
module Frontend.SemanticAnalysis.Runner where

import Prelude hiding (cycle)
import Control.Monad.State
import qualified Data.Map as M

import Language.BuiltIns
import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.Monad
import Utility.PrettyPrinting

import Frontend.SemanticAnalysis.Checks.CyclicInherritance
import Frontend.SemanticAnalysis.Checks.UniqueIdents
import Frontend.SemanticAnalysis.ContextUpdates.BuildEnv
import Frontend.SemanticAnalysis.Checks.NoNestedDecls
import Frontend.SemanticAnalysis.Transformations.UnifyOperators
import Frontend.SemanticAnalysis.Checks.TypeCorrectness
import Frontend.SemanticAnalysis.Transformations.ConstantPropagation hiding ( initialState )
import Frontend.SemanticAnalysis.Transformations.SimpleElimination
import Frontend.SemanticAnalysis.Checks.ProperReturnStatements

import Backend.CodeEmitter.Transformations.UnifyVariables -- TODO maybe move to backend runer



checkProgram :: Program -> CheckM Program
checkProgram p0 = do
  -- set initial state
  put initialState

  -- first pass of checks
  mapM_ (\c -> c p0) [ checkCI, checkIdentsUnique, checkNND ]
  liftIO $ putStrLn "[ OK ] First pass of static checks."

  -- transformation of AST
  let p1 = unifyOperators p0

  liftIO $ putStrLn (show p1)

  -- bulding type environment for second pass of checks
  buildEnv p1
  liftIO $ putStrLn "\r[ OK ] Generating environment of types."

  -- second pass of checks
  p2 <- checkTC p1
  p3 <- propagateConstants p2
  -- p3 <- simpleElimination p2
  checkRS p3
  let p4 = unifyVariables p3

  liftIO $ putStrLn "[ OK ] Second pass of static checks."
  liftIO $ putStrLn (printTree p4)

  return p4

initialState :: CheckState
initialState = CheckState initialEnv undefined (M.empty) Normal
