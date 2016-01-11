{-# LANGUAGE GADTs #-}
module Backend.CodeEmitter.Emitter where

import Control.Lens
import Control.Monad

import Frontend.Parser.AbsLatte
import Backend.CodeEmitter.DataTypes
import Backend.CodeEmitter.ASM
import Backend.CodeEmitter.Helpers
import qualified Data.Map as M


initialState :: CompilationState
initialState = CompilationState (BlockIdx 0) [] M.empty

emitProgram :: Program -> X86M ()
emitProgram = emitTree

emitTree :: Tree a -> X86M ()
emitTree x@(Program _) =
  composOpM_ emitTree x

emitTree x@(FnDef _ (Ident ident) _ _) = do
  localOffsetEnv .= getFrameOffsets x
  localsOffset <- getStackOffset

  placeLabel $ Label ident

  pushl ebp
  movl  esp ebp
  unless (localsOffset == LImm 0) $
    subl  localsOffset esp

  composOpM_ emitTree x

  movl  ebp esp
  popl  ebp
  ret

emitTree x = composOpM_ emitTree x

--emitFunction :: FnDef -> X86M ()
--emitFunction = undefined