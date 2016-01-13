{-# LANGUAGE GADTs #-}
module Backend.X86.Emitter where

import Control.Lens hiding ( op )

import Language.BuiltIns
import Frontend.Parser.AbsLatte
import Utility.PrettyPrinting
import Backend.X86.DataTypes
import Backend.X86.ASM
import Backend.X86.Helpers
import qualified Data.Map as M


initialState :: CompilationState
initialState = CompilationState
  0
  []
  []
  M.empty
  (Ident "")
  (Nothing)
  0

emitProgram :: Program -> X86M ()
emitProgram = emitTree

emitTree :: Tree a -> X86M ()
emitTree x@(Program _) = do
  preambleStmts %= (SDirective (DGlobl "main"):)
  composOpM_ emitTree x

emitTree x@(FnDef _ ident _ block) = do
  resetLabelIdx
  localOffsetEnv .= getFrameOffsets x
  functionName   .= ident
  localsOffset <- getStackOffset

  commentBegin
  getBeginLabel >>= placeLabel

  pushl ebp
  movl  esp ebp
  subl  localsOffset esp

  blankLine
  emitTree block

  getEndLabel >>= placeLabel

  movl  ebp esp
  popl  ebp
  ret

emitTree (Init ident e1) = do
  loc  <- getLocOf (LVar ident)
  emitExpr e1
  popl eax
  movl eax loc

emitTree (Ass lval e1) = do
  loc <- getLocOf lval
  emitExpr e1
  popl eax
  movl eax loc

emitTree (Incr lval) = do
  loc <- getLocOf lval
  addl (LImm 1) loc

emitTree (Decr lval) = do
  loc <- getLocOf lval
  subl (LImm 1) loc

emitTree (Ret e1) = do
  comment "Prepare to retun a value"
  emitExpr e1
  popl eax
  getEndLabel >>= jmp.LLbl
  comment "End return"

emitTree (VRet) = do
  getEndLabel >>= jmp.LLbl

emitTree (Cond e1 s1) = do
  lEnd    <- newIndexedLabel "if_end"

  emitExpr e1
  popl eax
  test eax eax
  je (LLbl lEnd)

  emitTree s1

  placeLabel lEnd

emitTree (CondElse e1 s1 s2) = do
  lElse <- newIndexedLabel "if_else"
  lEnd  <- newIndexedLabel "if_end"

  emitExpr e1
  popl eax
  test eax eax
  je (LLbl lElse)

  emitTree s1
  jmp (LLbl lEnd)

  placeLabel lElse

  emitTree s2
  placeLabel lEnd

emitTree (While e1 s1) = do
  lCond <- newIndexedLabel "while_cond"
  lBody <- newIndexedLabel "while_body"

  jmp (LLbl lCond)

  placeLabel lBody
  emitTree s1

  placeLabel lCond
  emitExpr e1
  popl eax
  test eax eax
  jne (LLbl lBody)

emitTree (For{}) = error "Arrays not supported yet"

emitTree (SExp e1) = do
  emitExpr e1
  popl eax

emitTree x = composOpM_ emitTree x


getLocOf :: LVal -> X86M Loc
getLocOf (LVar ident) = do
  off <- uses localOffsetEnv (M.!ident)
  return (LFrRel off)

getLocOf _ = error "Objects / arrays not supported yet"


emitExpr :: Expr -> X86M ()
emitExpr (ELitInt n) =
  pushl $ LImm $ fromIntegral n

emitExpr (ELitTrue) =
  pushl $ LImm 1

emitExpr (ELitFalse) =
  pushl $ LImm 0

emitExpr (ELitNull{}) =
  pushl $ LAbs _NULL

emitExpr (EString s) = do
  label <- declareString s
  pushl $ LStr label

emitExpr (EApp ident args) = do
  let label = functionLabel ident
  comment $ ">> " ++ printTree ident ++ "()"
  subl (LImm argOffset) esp

  mapM_ computeArg $ zip [0..] args
  call (LLbl label)
  blankLine

  addl (LImm argOffset) esp
  comment $ "<< " ++ printTree ident ++ "()"

  pushl eax
  where
    argOffset = length args * varSize
    computeArg (n, e1) = do
      let argloc = LRel ESP $ PointerOffset ((-n) * varSize)
      emitExpr e1
      popl eax
      movl eax argloc

emitExpr (ELVal lval) = do
  loc <- getLocOf lval
  movl  loc eax
  pushl eax

emitExpr (Neg e1) = do
  emitExpr e1
  popl     eax
  neg      eax
  pushl    eax

emitExpr (Not e1) = do
  emitExpr e1
  popl ecx
  movl (LImm 1) eax
  subl ecx eax
  pushl eax

emitExpr x@(EBinOp e1 op e2) = case op of
  Plus_Str -> emitExpr $ EApp concatIdent [e1, e2]

  Plus_Int -> emitIntOp x
  Minus    -> emitIntOp x
  Times    -> emitIntOp x
  Div      -> emitIntOp x
  Mod      -> emitIntOp x

  EQU_Str  -> emitBooleanOp x
  NE_Str   -> emitBooleanOp x
  EQU_Int  -> emitBooleanOp x
  NE_Int   -> emitBooleanOp x
  EQU_Bool -> emitBooleanOp x
  NE_Bool  -> emitBooleanOp x
  LTH      -> emitBooleanOp x
  LE       -> emitBooleanOp x
  GTH      -> emitBooleanOp x
  GE       -> emitBooleanOp x
  EQU      -> emitBooleanOp x
  NE       -> emitBooleanOp x
  AND      -> emitBooleanOp x
  OR       -> emitBooleanOp x

  _        -> error $ "Unsupported operator " ++ show op

emitExpr x = error $ "Unsupported expression " ++ show x

emitIntOp :: Expr -> X86M ()
emitIntOp (EBinOp e1 op e2) = do
  emitExpr e1
  emitExpr e2
  popl ecx
  popl eax

  case op of
    Plus_Int ->                      addl  ecx eax  >> pushl eax
    Minus    ->                      subl  ecx eax  >> pushl eax
    Times    ->                      imull ecx eax  >> pushl eax
    Div      -> movl (LImm 0) edx >> idivl ecx      >> pushl eax
    Mod      -> movl (LImm 0) edx >> idivl ecx      >> pushl edx
    _        -> error "Int op was expected"

emitIntOp _ = error "EBinOp was expected"

emitBooleanOp :: Expr -> X86M ()
emitBooleanOp (EBinOp e1 op e2) = do
  emitExpr e1
  emitExpr e2
  popl  ecx
  popl  eax
  case op of
    EQU_Str  -> referenceEquality
    NE_Str   -> referenceNEquality
    EQU_Int  -> referenceEquality
    NE_Int   -> referenceNEquality
    EQU_Bool -> referenceEquality
    NE_Bool  -> referenceNEquality
    LTH      -> comparison setl
    LE       -> comparison setle
    GTH      -> comparison setg
    GE       -> comparison setge
    AND      -> lAnd
    OR       -> lOr
    _        -> error "Boolean op was expected"
  where
    referenceEquality :: X86M ()
    referenceEquality = do
      movl  (LImm 0) edx
      cmpl  eax ecx
      sete  dl
      pushl edx
    referenceNEquality :: X86M ()
    referenceNEquality = do
      movl  (LImm 0) edx
      cmpl  eax ecx
      sete  dl
      pushl edx
    comparison :: (WriteInstr1A) -> X86M ()
    comparison instr = do
      movl  (LImm 0) edx
      cmpl  ecx eax
      instr dl
      pushl edx
    lAnd = do
      andl ecx eax
      pushl eax
    lOr = do
      orl ecx eax
      pushl eax

emitBooleanOp _ = error "EBinOp was expected"



