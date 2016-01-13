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
  lTrue <- newIndexedLabel "if_true"
  lEnd  <- newIndexedLabel "if_end"

  emitBooleanExpr e1 lTrue lEnd lTrue

  placeLabel lTrue
  emitTree s1

  placeLabel lEnd

emitTree (CondElse e1 s1 s2) = do
  lTrue  <- newIndexedLabel "if_true"
  lFalse <- newIndexedLabel "if_false"
  lEnd   <- newIndexedLabel "if_end"

  emitBooleanExpr e1 lTrue lFalse lTrue

  placeLabel lTrue
  emitTree   s1
  jmp        (LLbl lEnd)

  placeLabel lFalse
  emitTree   s2

  placeLabel lEnd

emitTree (While e1 s1) = do
  lCond <- newIndexedLabel "while_cond"
  lBody <- newIndexedLabel "while_body"
  lEnd  <- newIndexedLabel "while_end"

  jmp (LLbl lCond)

  placeLabel lBody
  emitTree s1

  placeLabel lCond
  emitBooleanExpr e1 lBody lEnd lEnd

  placeLabel lEnd

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
      let argloc = LRel ESP $ PointerOffset (n * varSize)
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
  storeBoolean 0 1 e1

emitExpr x@(EBinOp e1 op e2) = case op of
  Plus_Str -> emitExpr $ EApp concatIdent [e1, e2]

  Plus_Int -> emitIntOp x
  Minus    -> emitIntOp x
  Times    -> emitIntOp x
  Div      -> emitIntOp x
  Mod      -> emitIntOp x

  _ | (any (op==)
      [EQU_Str, NE_Str, EQU_Int, NE_Int,
      EQU_Bool, NE_Bool, LTH, LE, GTH, GE, AND, OR])
        -> storeBoolean 1 0 x

  _        -> error $ "Unsupported operator " ++ show op

emitExpr x = error $ "Unsupported expression " ++ show x

storeBoolean :: Int -> Int -> Expr -> X86M ()
storeBoolean vTrue vFalse e1 = do
  lTrue  <- newIndexedLabel "store_true"
  lFalse <- newIndexedLabel "store_false"
  lAfter <- newIndexedLabel "store_after"

  emitBooleanExpr e1 lTrue lFalse lTrue

  placeLabel lTrue
  pushl      (LImm vTrue)
  jmp        (LLbl lAfter)
  placeLabel lFalse
  pushl      (LImm vFalse)
  placeLabel lAfter

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

-- Adapted from slides for the lecture
emitBooleanExpr :: Expr -> Label -> Label -> Label -> X86M ()
emitBooleanExpr (EBinOp e1 op e2) lTrue lFalse lNext = do
  case op of
    EQU_Str  -> referenceEquality
    NE_Str   -> referenceNEquality
    EQU_Int  -> referenceEquality
    NE_Int   -> referenceNEquality
    EQU_Bool -> referenceEquality
    NE_Bool  -> referenceNEquality
    LTH      -> comparison jl    jge
    LE       -> comparison jle   jg
    GTH      -> comparison jg    jle
    GE       -> comparison jge   jl
    AND      -> lAnd
    OR       -> lOr
    _        -> error "Boolean op was expected"
  where
    loadValues :: X86M ()
    loadValues = do
      emitExpr e1
      emitExpr e2
      popl  ecx
      popl  eax
    referenceEquality :: X86M ()
    referenceEquality = do
      loadValues
      cmpl    eax ecx
      genJump je  jne
    referenceNEquality :: X86M ()
    referenceNEquality = do
      loadValues
      cmpl    eax ecx
      genJump jne je
    comparison :: WriteInstr1A -> WriteInstr1A -> X86M ()
    comparison posJ negJ = do
      loadValues
      cmpl    ecx eax
      genJump posJ negJ
    lAnd = do
      lMiddle <- newIndexedLabel "and"
      emitBooleanExpr e1 lMiddle lFalse lMiddle
      placeLabel lMiddle
      emitBooleanExpr e2 lTrue   lFalse lNext
    lOr = do
      lMiddle <- newIndexedLabel "or"
      emitBooleanExpr e1 lTrue   lMiddle lMiddle
      placeLabel lMiddle
      emitBooleanExpr e2 lTrue   lFalse  lNext
    genJump :: WriteInstr1A -> WriteInstr1A -> X86M ()
    genJump posJump negJump
      | lNext == lTrue  = negJump (LLbl lFalse)
      | lNext == lFalse = posJump (LLbl lTrue )
      | otherwise       = posJump (LLbl lTrue ) >> jmp (LLbl lFalse)

emitBooleanExpr (Not e1) lTrue lFalse lNext =
  emitBooleanExpr e1 lFalse lTrue lNext

emitBooleanExpr _ _ _ _ = error "Boolean expression was expected"


