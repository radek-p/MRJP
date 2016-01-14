module Backend.X86.DataTypes where

import Control.Monad.State
import Control.Lens
import qualified Data.Map as M

import Frontend.Parser.AbsLatte


data CompilationState =
  CompilationState
    Int            -- number to generate a label for string
    [Statement]    -- main program
    [Statement]    -- preamble statements (string definitions)
    LocalOffsetEnv
    Ident          -- current function name
    (Maybe Ident)  -- current class name
    Int            -- numbr ot generate other labels

type LocalOffsetEnv = M.Map Ident PointerOffset

nextStringIdx  :: Lens' CompilationState Int
emittedStmts   :: Lens' CompilationState [Statement]
preambleStmts  :: Lens' CompilationState [Statement]
localOffsetEnv :: Lens' CompilationState LocalOffsetEnv
functionName   :: Lens' CompilationState Ident
className      :: Lens' CompilationState (Maybe Ident)
nextLabelIdx   :: Lens' CompilationState Int

nextStringIdx  = lens (\(CompilationState a _ _ _ _ _ _) -> a) (\(CompilationState _ b c d e f g) a -> CompilationState a b c d e f g)
emittedStmts   = lens (\(CompilationState _ b _ _ _ _ _) -> b) (\(CompilationState a _ c d e f g) b -> CompilationState a b c d e f g)
preambleStmts  = lens (\(CompilationState _ _ c _ _ _ _) -> c) (\(CompilationState a b _ d e f g) c -> CompilationState a b c d e f g)
localOffsetEnv = lens (\(CompilationState _ _ _ d _ _ _) -> d) (\(CompilationState a b c _ e f g) d -> CompilationState a b c d e f g)
functionName   = lens (\(CompilationState _ _ _ _ e _ _) -> e) (\(CompilationState a b c d _ f g) e -> CompilationState a b c d e f g)
className      = lens (\(CompilationState _ _ _ _ _ f _) -> f) (\(CompilationState a b c d e _ g) f -> CompilationState a b c d e f g)
nextLabelIdx   = lens (\(CompilationState _ _ _ _ _ _ g) -> g) (\(CompilationState a b c d e f _) g -> CompilationState a b c d e f g)

type X86M a = StateT CompilationState (IO) a

data Reg = EAX | EBX | ECX | EDX | ESI | EDI | EBP | EIP | ESP | DL deriving Eq

newtype PointerOffset = PointerOffset { getPointerOffset :: Int    } deriving Eq
newtype Pointer       = Pointer       { getPointer       :: Int    } deriving Eq
newtype Label         = Label         { getLabel         :: String } deriving Eq

data Loc
  = LReg   Reg                   -- CPU registry
  | LFrRel PointerOffset         -- Address relative to %ebp
  | LAbs   Pointer               -- Absolute address
  | LRel   Reg PointerOffset     -- Absolute + offset (used for class field access)
  | LImm   Int                   -- Immediate value
  | LLbl   Label                 -- Address pointed by label
  | LStr   Label
  deriving Eq

isImmediate :: Loc -> Bool
isImmediate LReg{} = True
isImmediate LImm{} = True
isImmediate LLbl{} = True
isImmediate LStr{} = True
isImmediate _      = False

data Statement
  = SInstr     Instr
  | SLabel     Label
  | SDirective Directive
  | SComment   Bool String

data Directive
  = DString    String
  | DGlobl     String

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
  | IMulL
  | IDivL
  | PushL
  | PopL
  | Ret_
  | Test
  | Cdq
  | Neg_
  | Call
  | CmpL
  | AndL
  | OrL
  | Jne | Jmp | Je  | JG  | JGE | JL  | JLE
