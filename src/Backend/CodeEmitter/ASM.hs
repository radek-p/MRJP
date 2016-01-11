module Backend.CodeEmitter.ASM where

import Control.Lens hiding ( op )

import Backend.CodeEmitter.DataTypes


type WriteInstr0A =               X86M ()
type WriteInstr1A =        Loc -> X86M ()
type WriteInstr2A = Loc -> Loc -> X86M ()

emit :: Statement -> X86M ()
emit stmt =
  emittedStmts %= (stmt:)

emitI :: Instr -> X86M ()
emitI instr =
  emit $ SInstr instr

emit0A :: Operation -> WriteInstr0A
emit0A op       = emitI $ Instr0A op

emit1A :: Operation -> WriteInstr1A
emit1A op l1    = emitI $ Instr1A op l1

emit2A :: Operation -> WriteInstr2A
emit2A op l1 l2 = emitI $ Instr2A op l1 l2

esp, ebp, eax, ebx, ecx, edx :: Loc
[esp, ebp, eax, ebx, ecx, edx] = map LReg [ESP, EBP, EAX, EBX, ECX, EDX]

ret :: WriteInstr0A
[ret] = map emit0A [Ret]

pushl, popl :: WriteInstr1A
[pushl, popl] = map emit1A [PushL, PopL]

movl, addl, subl :: WriteInstr2A
[movl, addl, subl] = map emit2A [MovL, AddL, SubL]

placeLabel :: Label -> X86M ()
placeLabel label =
  emit $ SLabel label

newIndexedLabel :: String -> X86M Label
newIndexedLabel str = do
  BlockIdx idx <- use nextBlockIdx
  nextBlockIdx .= BlockIdx (idx + 1)
  return $ Label (str ++ show idx)

resetLabelIdx :: X86M ()
resetLabelIdx =
  nextBlockIdx .= BlockIdx 0


