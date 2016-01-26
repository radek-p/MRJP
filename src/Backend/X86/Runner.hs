{-# LANGUAGE GADTs #-}
module Backend.X86.Runner where

import Control.Monad.State
import Control.Lens

import Language.BuiltIns
import Utility.PrettyPrinting
import Frontend.Parser.AbsLatte
import Backend.X86.DataTypes
import Backend.X86.Emitter
import Backend.X86.GasmPrint
import Backend.X86.Transformations.UnifyVariables
import Backend.X86.Transformations.InitialisationOfVariables
import Backend.X86.Transformations.DesugaringOfForLoops
--import Backend.X86.ASMTransformations.SimplifyStackOperations


----------------------------------------------------------------------------
-- Transform a tree to a string containing a sequence of GASM statements  --
----------------------------------------------------------------------------

genASM :: Env -> Program -> IO String
genASM tenv p1 = do
  liftIO $ putStrLn "[ 0/2 ] Compilation."

  -- Preprocessing of program tree
  let p2 = desugarForLoops . initializeAllVariables $ p1
  p3    <- unifyVariables p2

  liftIO $ putStrLn "[ 1/2 ] Compilation."
  -- liftIO $ putStrLn $ indent 8 $ printWhite $ "Transformend program tree:"
  -- liftIO $ putStrLn $ indent 8 $ printGreen $ printTree p3

  -- Compilation
  st <- execStateT (emitProgram p3) (initialState tenv)

  let stmts    = reverse $ st ^. emittedStmts
  let preamble = reverse $ st ^. preambleStmts

  let stmts'   = stmts -- simplifyStackOperations stmts

  liftIO $ putStrLn "[ 2/2 ] Compilation."

  return $ concat (map printGasm preamble) ++ concat (map printGasm stmts')