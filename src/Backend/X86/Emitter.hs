{-# LANGUAGE GADTs #-}
module Backend.X86.Emitter where

import Control.Lens

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
emitTree x@(Program _) =
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
  res <- emitExpr e1
  loc <- getLocOf (LVar ident)
  movl res loc

emitTree (Ass lval e1) = do
  res <- emitExpr e1
  loc <- getLocOf lval
  movl res loc

emitTree (Incr lval) = do
  loc <- getLocOf lval
  addl (LImm 1) loc

emitTree (Decr lval) = do
  loc <- getLocOf lval
  subl (LImm 1) loc

emitTree (Ret e1) = do
  res <- emitExpr e1
  movl res eax
  getEndLabel >>= jmp.LLbl

emitTree (VRet) = do
  getEndLabel >>= jmp.LLbl

emitTree (Cond e1 s1) = do
  lEnd    <- newIndexedLabel "if_end"

  res <- emitExpr e1
  test res res
  je (LLbl lEnd)

  emitTree s1

  placeLabel lEnd

emitTree (CondElse e1 s1 s2) = do
  lElse <- newIndexedLabel "if_else"
  lEnd  <- newIndexedLabel "if_end"

  res <- emitExpr e1
  test res res
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
  res <- emitExpr e1
  test res res
  jne (LLbl lBody)

emitTree (For{}) = error "Arrays not supported yet"

emitTree (SExp e1) =
  emitExpr e1 >> return ()

emitTree x = composOpM_ emitTree x

getLocOf :: LVal -> X86M Loc
getLocOf (LVar ident) = do
  off <- uses localOffsetEnv (M.!ident)
  return (LFrRel off)

getLocOf _ = error "Objects / arrays not supported yet"

emitExpr :: Expr -> X86M Loc
emitExpr (ELitInt n) =
  return $ LImm $ fromIntegral n

emitExpr (ELitTrue) =
  return $ LImm 1

emitExpr (ELitFalse) =
  return $ LImm 0

emitExpr (ELitNull{}) =
  return $ LAbs _NULL

emitExpr (EString s) = do
  label <- declareString s
  return $ LLbl label

emitExpr (EApp ident args) = do
  let label = functionLabel ident
  comment $ ">> " ++ printTree ident ++ "()"
  subl (LImm argOffset) esp

  mapM_ computeArg $ zip [0..] args
  call (LLbl label)
  blankLine

  addl (LImm argOffset) esp
  comment $ "<< " ++ printTree ident ++ "()"

  return eax
  where
    argOffset = length args * varSize
    computeArg (n, e1) = do
      let argloc = LRel ESP $ PointerOffset ((-n) * varSize)
      argloc0 <- emitExpr e1
      movl argloc0 argloc
      return argloc

emitExpr (ELVal lval) = do
  loc <- getLocOf lval
  movl loc eax
  return eax

emitExpr (Neg e1) = do
  loc <- emitExpr e1
  case loc of
    LImm n -> return $ LImm (-n)
    _      -> do
      neg loc
      return loc

emitExpr (Not e1) = do
  loc <- emitExpr e1
  -- TODO !!
  return loc

emitExpr _ = do
  comment "TODO Expr"
  return eax