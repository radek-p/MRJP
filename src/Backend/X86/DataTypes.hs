module Backend.X86.DataTypes where

import Control.Monad.State
import Control.Lens
import qualified Data.Map as M

import Frontend.Parser.AbsLatte
import Language.BuiltIns


data CompilationState =
  CompilationState
    Int            -- number to generate a label for string
    [Statement]    -- main program
    [Statement]    -- preamble statements (string definitions)
    LocalOffsetEnv
    Ident          -- current function name
    (Maybe Ident)  -- current class name
    Int            -- numbr ot generate other labels
    Env            -- environment

type LocalOffsetEnv = M.Map Ident PointerOffset

nextStringIdx  :: Lens' CompilationState Int
emittedStmts   :: Lens' CompilationState [Statement]
preambleStmts  :: Lens' CompilationState [Statement]
localOffsetEnv :: Lens' CompilationState LocalOffsetEnv
functionName   :: Lens' CompilationState Ident
className      :: Lens' CompilationState (Maybe Ident)
nextLabelIdx   :: Lens' CompilationState Int
env            :: Lens' CompilationState Env

nextStringIdx  = lens (\(CompilationState a _ _ _ _ _ _ _) -> a) (\(CompilationState _ b c d e f g h) a -> CompilationState a b c d e f g h)
emittedStmts   = lens (\(CompilationState _ b _ _ _ _ _ _) -> b) (\(CompilationState a _ c d e f g h) b -> CompilationState a b c d e f g h)
preambleStmts  = lens (\(CompilationState _ _ c _ _ _ _ _) -> c) (\(CompilationState a b _ d e f g h) c -> CompilationState a b c d e f g h)
localOffsetEnv = lens (\(CompilationState _ _ _ d _ _ _ _) -> d) (\(CompilationState a b c _ e f g h) d -> CompilationState a b c d e f g h)
functionName   = lens (\(CompilationState _ _ _ _ e _ _ _) -> e) (\(CompilationState a b c d _ f g h) e -> CompilationState a b c d e f g h)
className      = lens (\(CompilationState _ _ _ _ _ f _ _) -> f) (\(CompilationState a b c d e _ g h) f -> CompilationState a b c d e f g h)
nextLabelIdx   = lens (\(CompilationState _ _ _ _ _ _ g _) -> g) (\(CompilationState a b c d e f _ h) g -> CompilationState a b c d e f g h)
env            = lens (\(CompilationState _ _ _ _ _ _ _ h) -> h) (\(CompilationState a b c d e f g _) h -> CompilationState a b c d e f g h)

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
  | LRel2  Reg PointerOffset Loc Int -- Absolute: offset(register, index, size)
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
  | DLong      Label
  | DArrayHeader String Int

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
  | LeaL
  | Jne | Jmp | Je  | JG  | JGE | JL  | JLE
