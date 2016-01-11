module Backend.CodeEmitter.DataTypes where

import Control.Monad.State
import Control.Lens
import qualified Data.Map as M
import Frontend.Parser.AbsLatte

data CompilationState =
  CompilationState BlockIdx [Statement] LocalOffsetEnv

type LocalOffsetEnv = M.Map Ident PointerOffset

emittedStmts :: Lens' CompilationState [Statement]
emittedStmts = lens (\(CompilationState _ b _) -> b) (\(CompilationState a _ c) b -> CompilationState a b c)

nextBlockIdx :: Lens' CompilationState BlockIdx
nextBlockIdx = lens (\(CompilationState a _ _) -> a) (\(CompilationState _ b c) a -> CompilationState a b c)

localOffsetEnv :: Lens' CompilationState LocalOffsetEnv
localOffsetEnv = lens (\(CompilationState _ _ c) -> c) (\(CompilationState a b _) c -> CompilationState a b c)

type X86M a = StateT CompilationState (IO) a

data Reg = EAX | EBX | ECX | EDX | ESI | EDI | EBP | EIP | ESP deriving Eq

newtype BlockIdx      = BlockIdx      { getBlockIdx      :: Int    }
newtype PointerOffset = PointerOffset { getPointerOffset :: Int    } deriving Eq
newtype Pointer       = Pointer       { getPointer       :: Int    } deriving Eq
newtype Label         = Label         { getLabel         :: String }

data Loc
  = LReg   Reg                   -- CPU registry
  | LFrRel PointerOffset         -- Address relative to %ebp
  | LAbs   Pointer               -- Absolute address
  | LRel   Pointer PointerOffset -- Absolute + offset (used for class field access)
  | LImm   Int                   -- Immediate value
  deriving Eq

data Statement
  = SInstr     Instr
  | SLabel     Label
  | SDirective -- TODO Remove later if not necessary

data Instr
  = Instr0A
      Operation
  | Instr1A
      Operation
      Loc
  | Instr2A
      Operation -- Defines semantics of the instruction
      Loc       -- Data location
      Loc       -- Data / result location

data Operation
  = MovL
  | AddL
  | SubL
  | Jne
  | Jmp
  | PushL
  | PopL
  | Ret

