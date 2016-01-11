{-# LANGUAGE GADTs #-}
module Backend.CodeEmitter.Runner where

import Control.Monad.State

import Frontend.Parser.AbsLatte
import Backend.CodeEmitter.DataTypes
--import Backend.CodeEmitter.Transformations.UnifyVariables
import Backend.CodeEmitter.Emitter
import Backend.CodeEmitter.GasmPrint


genASM :: Program -> IO String
genASM x = do
  (CompilationState _ stmts _) <- execStateT (emitProgram x) initialState
  return $ concat (map printGasm (reverse stmts))