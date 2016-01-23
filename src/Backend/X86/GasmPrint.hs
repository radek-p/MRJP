module Backend.X86.GasmPrint where

import Utility.PrettyPrinting
import Backend.X86.DataTypes


class GasmPrint a where
  printGasm :: a -> String

instance GasmPrint Statement where
  printGasm (SInstr instr)   = (indent' $ printGasm instr) ++ "\n"
  printGasm (SLabel label)   = "\n" ++ printGasm label ++ ":\n"
  printGasm (SDirective d)   = printGasm d ++ "\n"
  printGasm (SComment _ [])  = "\n"
  printGasm (SComment i c)   = if i then (indent' $ prefixLines "# " c) ++ "\n" else "\n/* " ++ c ++ " */"

instance GasmPrint Instr where
  printGasm (Instr0A op)       =
    (printGasm op)
  printGasm (Instr1A op l1)    =
    align (printGasm op) ++ "  " ++ (printGasm l1)
  printGasm (Instr2A op l1 l2) =
    align (printGasm op) ++ "  " ++ align (printGasm l1) ++ ", " ++ (printGasm l2)

instance GasmPrint Directive where
  printGasm (DString s) = indent' ".string " ++ show s
  printGasm (DGlobl  s) = ".globl " ++ s

instance GasmPrint Operation where
  printGasm op = case op of
    MovL  -> "movl"
    AddL  -> "addl"
    SubL  -> "subl"
    Jne   -> "jne"
    Jmp   -> "jmp"
    PushL -> "pushl"
    PopL  -> "popl"
    Ret_  -> "ret"
    Cdq   -> "cdq"
    Test  -> "test"
    Je    -> "je"
    Neg_  -> "neg"
    Call  -> "call"
    IMulL -> "imull"
    IDivL -> "idivl"
    CmpL  -> "cmpl"
    JG    -> "jg"
    JGE   -> "jge"
    JL    -> "jl"
    JLE   -> "jle"
    AndL  -> "andl"
    OrL   -> "orl"
    LeaL  -> "leal"

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
    DL  -> "dl"

instance GasmPrint Pointer where
  printGasm (Pointer n) = show n

instance GasmPrint PointerOffset where
  printGasm (PointerOffset n) = show n

instance GasmPrint Loc where
  printGasm l = case l of
    LReg   r       -> printGasm r
    LFrRel off     -> printGasm off ++ "(%ebp)"
    LAbs   ptr     -> "$" ++ printGasm ptr
    LRel   ptr off -> (if (off == PointerOffset 0) then "" else printGasm off) ++ "(" ++ printGasm ptr ++ ")"
    LRel2  reg off reg2 sz -> (if (off == PointerOffset 0) then "" else printGasm off) ++
                              "(" ++ printGasm reg ++ ", " ++ printGasm reg2 ++ ", " ++ show sz ++ ")"
    LImm   n       -> "$" ++ show n
    LLbl   lbl     -> printGasm lbl
    LStr   lbl     -> "$" ++ printGasm lbl

instance GasmPrint Label where
  printGasm (Label l) = l

defaultAlign :: Int
defaultAlign = 8

alignTo :: Int -> String -> String
alignTo n s =
  s ++ [ ' ' | _ <- [1..(n - length s)] ]

align :: String -> String
align = alignTo defaultAlign

indent' :: String -> String
indent' = indent defaultAlign
