{-# LANGUAGE GADTs #-}
module Backend.X86.ASM where

import Control.Lens hiding ( op )

import Language.BuiltIns
import Backend.X86.DataTypes
import Frontend.Parser.AbsLatte
import Utility.PrettyPrinting


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

esp, ebp, eax, ebx, ecx, edx, dl :: Loc
[esp, ebp, eax, ebx, ecx, edx, dl] = map LReg [ESP, EBP, EAX, EBX, ECX, EDX, DL]

ret, cdq :: WriteInstr0A
[ret, cdq] = map emit0A [Ret_, Cdq]

pushl, popl, jmp, je, jne, neg, call, idivl, jg, jge, jl, jle :: WriteInstr1A
[pushl, popl, jmp, je, jne, neg, call, idivl, jg, jge, jl, jle] =
  map emit1A [PushL, PopL, Jmp, Je, Jne, Neg_, Call, IDivL, JG, JGE, JL, JLE]

movl, addl, subl, test, imull, cmpl, andl, orl, leal :: WriteInstr2A
[movl, addl, subl, test, imull, cmpl, andl, orl, leal] = map emit2A [MovL, AddL, SubL, Test, IMulL, CmpL, AndL, OrL, LeaL]

_NULL :: Pointer
_NULL = Pointer 0

topComment :: String -> X86M ()
topComment s =
  emit $ SComment False s

comment :: String -> X86M ()
comment s =
  emit $ SComment True s

blankLine :: X86M ()
blankLine = comment ""

commentBegin :: X86M ()
commentBegin = do
  ident <- use functionName
  let str = "Function: " ++ (printTree ident)
  let lin = [ '-' | _ <- str ]
  topComment $ "+-" ++ lin ++ "-+\n   | " ++ str ++ " |\n   +-" ++ lin ++ "-+"

mangleFunction :: Ident -> String
mangleFunction (Ident s) =
  "" ++ s -- Mangling is disabled for now, it may be turned on when adding object support.

functionLabel :: Ident -> Label
functionLabel ident = Label $ mangleFunction ident

labelPrefix :: X86M String
labelPrefix = do
  ident <- use functionName
  clsid <- use className
  return $ labelPrefix' ident clsid

labelPrefix' :: Ident -> Maybe Ident -> String
labelPrefix' ident clsid =
  let fid = mangleFunction ident in
  case clsid of
    Just cls -> printTree (getIdent cls) ++ "." ++ fid
    Nothing  -> fid

placeLabel :: Label -> X86M ()
placeLabel label =
  emit $ SLabel label

getBeginLabel :: X86M Label
getBeginLabel = do
  pref <- labelPrefix
  return $ Label pref

getEndLabel :: X86M Label
getEndLabel = do
  pref <- labelPrefix
  return $ Label $ "." ++ pref ++ ".return"

getVtableLabel :: X86M Label
getVtableLabel = do
  mclsid <- use className
  case mclsid of
    Just clsid -> return $ getVtableLabel' clsid
    Nothing    -> error "Internal error"

getVtableLabel' :: Ident -> Label
getVtableLabel' (Ident idstr) = Label $ "vtable." ++ idstr

newIndexedLabel :: String -> X86M Label
newIndexedLabel str = do
  idx <- use nextLabelIdx
  pref <- labelPrefix
  nextLabelIdx .= idx + 1
  return $ Label ("." ++ pref ++ "." ++ str ++ "." ++ show idx)

newStringLabel :: X86M Label
newStringLabel = do
  idx <- use nextStringIdx
  nextStringIdx .= idx + 1
  return $ Label (".STRING_" ++ show idx)

resetLabelIdx :: X86M ()
resetLabelIdx =
  nextLabelIdx .=  0

declareString :: String -> X86M Label
declareString s = do
  label <- newStringLabel
  let stmts = [ SDirective $ DString s, SLabel $ label ]
  preambleStmts %= (stmts++)
  return $ label


