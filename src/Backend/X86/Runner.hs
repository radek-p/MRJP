{-# LANGUAGE GADTs #-}
module Backend.X86.Runner where

import Control.Monad.State
import Control.Lens

import Frontend.Parser.AbsLatte
import Utility.PrettyPrinting
import Backend.X86.DataTypes
import Backend.X86.Emitter
import Backend.X86.GasmPrint
import Backend.X86.Transformations.UnifyVariables
import Backend.X86.Transformations.InitialisationOfVariables


----------------------------------------------------------------------------
-- Transform a tree to a string containing a sequence of GASM statements  --
----------------------------------------------------------------------------

genASM :: Program -> IO String
genASM p1 = do
  liftIO $ putStrLn "[ 0/3 ] Compilation."

  -- Preprocessing of program tree
  let p2 = initializeAllVariables p1
  let p3 = unifyVariables p2

  liftIO $ putStrLn "[ 1/3 ] Compilation."
  liftIO $ putStrLn $ indent 8 $ printWhite $ "Transformend program tree:"
  liftIO $ putStrLn $ indent 8 $ printGreen $ printTree p3

  -- Compilation
  st <- execStateT (emitProgram p3) initialState

  liftIO $ putStrLn "[ 2/3 ] Compilation."

  -- Postprocessing of instructions
  let stmts    = map printGasm $ reverse $ st ^. emittedStmts
  let preamble = map printGasm $ reverse $ st ^. preambleStmts

  liftIO $ putStrLn "[ 3/3 ] Compilation."

  return $ concat preamble ++ concat stmts