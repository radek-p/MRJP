module Backend.CodeEmitter.GasmPrint where

import Backend.CodeEmitter.ASM
import Backend.CodeEmitter.DataTypes


class GasmPrint a where
  printGasm :: a -> String

instance GasmPrint Statement where
  printGasm (SInstr instr)   = space ++ printGasm instr ++ "\n"
  printGasm (SLabel label)   = "\n" ++ printGasm label ++ ":\n"
  printGasm (SDirective)     = "TODO directives\n"

instance GasmPrint Instr where
  printGasm (Instr0A op)       =
    (printGasm op)
  printGasm (Instr1A op l1)    =
    align (printGasm op) ++ "  " ++ (printGasm l1)
  printGasm (Instr2A op l1 l2) =
    align (printGasm op) ++ "  " ++ align (printGasm l1) ++ ", " ++ (printGasm l2)

instance GasmPrint Operation where
  printGasm op = case op of
    MovL  -> "movl"
    AddL  -> "addl"
    SubL  -> "subl"
    Jne   -> "jne"
    Jmp   -> "jmp"
    PushL -> "pushl"
    PopL  -> "popl"
    Ret   -> "ret"

instance GasmPrint Reg where
  printGasm r = "%" ++ case r of
    EAX -> "eax"
    EBX -> "ebx"
    ECX -> "ecx"
    EDX -> "edx"
    ESI -> "esi"
    EDI -> "edi"
    EBP -> "ebp"
    EIP -> "eip"
    ESP -> "esp"

instance GasmPrint Pointer where
  printGasm (Pointer n) = show n ++ "TODO!"

instance GasmPrint PointerOffset where
  printGasm (PointerOffset n) = show n

instance GasmPrint Loc where
  printGasm l = case l of
    LReg   r       -> printGasm r
    LFrRel off     -> printGasm off ++ "(%ebp)"
    LAbs   ptr     -> printGasm ptr
    LRel   ptr off -> printGasm off ++ "(" ++ printGasm ptr ++ ")"
    LImm   n       -> "$" ++ show n

instance GasmPrint Label where
  printGasm (Label l) = l

defaultAlign :: Int
defaultAlign = 8

alignTo :: Int -> String -> String
alignTo n s =
  s ++ [ ' ' | _ <- [1..(n - length s)] ]

align :: String -> String
align = alignTo defaultAlign

space :: String
space = align "" ++ "  "