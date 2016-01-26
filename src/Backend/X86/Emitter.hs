{-# LANGUAGE GADTs #-}
module Backend.X86.Emitter where

import Control.Lens hiding ( op )
import Control.Monad
import Control.Monad.State
import qualified Data.Map  as M
import qualified Data.List as L

import Language.BuiltIns
import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.ContextUpdates.BuildEnv
import Utility.PrettyPrinting
import Backend.X86.DataTypes
import Backend.X86.ASM
import Backend.X86.Helpers


initialState :: Env -> CompilationState
initialState tenv = CompilationState
  0
  []
  []
  M.empty
  (Ident "")
  (Nothing)
  0
  tenv

emitProgram :: Program -> X86M ()
emitProgram p = emitTree p >> emitVtables

withClass :: Ident -> X86M () -> X86M ()
withClass clsId a = do
  className .= Just clsId
  a
  className .= Nothing

emitTree :: Tree a -> X86M ()
emitTree x@(Program _) = do
  preambleStmts %= (SDirective (DGlobl "main"):)
  composOpM_ emitTree x

emitTree x@(ClsDefEx ident _ _) = withClass ident $ composOpM_ emitTree x
emitTree x@(ClsDef   ident   _) = withClass ident $ composOpM_ emitTree x

emitTree x@(FnDef _ ident _ block) = do
  resetLabelIdx
  -- if we're defining a method, we have to make place for the this pointer
  mcls <- use className
  let oenv = case mcls of { Just _ -> getFrameOffsets True x; Nothing -> getFrameOffsets False x }
  localOffsetEnv .= oenv
  functionName   .= ident
  localsOffset <- getStackOffset

  liftIO $ putStrLn $ "\n-----\nEmitting function " ++ show ident
  liftIO $ putStrLn $ "inClass: " ++ show mcls
  liftIO $ putStrLn $ "offset env " ++ show oenv
  liftIO $ putStrLn $ "locals offset " ++ show localsOffset

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
  getLocOf (LVar ident)
  emitExpr e1
  popl eax
  popl ecx
  movl eax (LRel ECX (PointerOffset 0))

emitTree (Ass lval e1) = do
  getLocOf lval
  emitExpr e1
  popl eax
  popl ecx
  movl eax (LRel ECX (PointerOffset 0))

emitTree (Incr lval) = do
  getLocOf lval
  popl ecx
  addl (LImm 1) (LRel ECX (PointerOffset 0))

emitTree (Decr lval) = do
  getLocOf lval
  popl ecx
  subl (LImm 1) (LRel ECX (PointerOffset 0))

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

emitTree (For{}) = error "For loops should be changed to whiles"

emitTree (SExp e1) = do
  emitExpr e1
  popl eax

emitTree x = composOpM_ emitTree x


getLocOf :: LVal -> X86M ()
getLocOf (LVar ident) = do
  off <- uses localOffsetEnv (\oenv -> M.findWithDefault (error $ "getLocOf: variable not found " ++ show ident ++ " in " ++ show oenv) ident oenv)
  leal   (LFrRel off) eax
  pushl  eax
  -- return (LFrRel off)

getLocOf (LArrAcc e1 e2) = do
  emitExpr e1
  emitExpr e2
  popl     ecx      -- array index
  popl     eax      -- pointer to array returned by e1
  leal     (LRel2 EAX (PointerOffset 0) ecx 4) eax
  pushl    eax

getLocOf (LTClsAcc t1 e2 i3) | isArrayType t1 = do
  when (i3 /= lengthIdent) $
    error "Internal error"      -- arrays so far have only one field
  emitExpr e2
  popl     eax
  subl     (LImm 4) eax
  pushl    eax

getLocOf (LTClsAcc t1 e2 i3) = do
  tenv <- use env
  let ClassT ident = t1
  let classes      = tenv ^. _3
  let cls          = M.findWithDefault (error $ "Class not found in " ++ show classes) ident classes
  let idx          = getFieldIndex i3 cls
  emitExpr e2
  popl     eax
  addl     (LImm $ idx * varSize) eax
  pushl    eax

getLocOf _ = error "Internal error: wrong LVal type in Emitter:hs"

getFieldIndex :: Ident -> Class -> Int
getFieldIndex _      Object = error "Internal error: field not found"
getFieldIndex ident (SubClass _ super _ fEnv _) =
  case M.lookup ident fEnv of
    Just (ClassField _ idx) -> idx
    Nothing                 -> getFieldIndex ident super

emitExpr :: Expr -> X86M ()
emitExpr (ELitInt n) =
  pushl $ LImm $ fromIntegral n

emitExpr (ELitTrue) =
  pushl $ LImm 1

emitExpr (ELitFalse) =
  pushl $ LImm 0

emitExpr (ELitNull{}) =
  pushl $ LImm 0

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
  getLocOf lval
  popl  ecx
  movl  (LRel ECX (PointerOffset 0)) eax
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
      EQU_Bool, NE_Bool, EQU_Arr, NE_Arr, EQU_Ref, NE_Ref, LTH, LE, GTH, GE, AND, OR])
        -> storeBoolean 1 0 x

  _        -> error $ "Unsupported operator " ++ show op

emitExpr (ArrAlloc _ e2) = do
  emitExpr e2
  popl  eax
  pushl eax
  addl  (LImm 1)  eax -- first entry - array length
  imull (LImm 4)  eax -- multiply the length by the size of array item (4 bytes)
  pushl eax
  pushl eax
  call  (LLbl $ Label "malloc")
  addl  (LImm 4)  esp
  pushl (LImm 0)
  pushl eax
  call  (LLbl $ Label "memset") -- initialize array with zeros
  addl  (LImm 12) esp
  popl  ecx
  movl  ecx       (LRel EAX (PointerOffset 0))
  addl  (LImm 4)  eax
  pushl eax

emitExpr (ClsAlloc ident) = do
  tenv <- use env
  let classes    = tenv ^. _3
  let fieldCount = getSize $ M.findWithDefault (error $ "Class not found in " ++ show classes) ident classes
  let objectSize = (fieldCount + 1) * varSize
  pushl (LImm objectSize)
  call  (LLbl $ Label "malloc")
  addl  (LImm 4) esp
  pushl (LImm objectSize)
  pushl (LImm 0)
  pushl eax
  call  (LLbl $ Label "memset")
  addl  (LImm 12) esp
  addl  (LImm 4)  eax
  pushl eax

emitExpr (TClsApply t1 e2 i3 args) = do
  let ClassT clsId   = t1
  tenv <- use env
  let classes        = tenv ^. _3
  let cls            = M.findWithDefault (error $ "Class not found in " ++ show classes) clsId classes
  let methods        = allMethods cls
  let Method _ idx _ = M.findWithDefault (error $ "Method " ++ show i3 ++ " not found in " ++ show classes) i3 methods

  comment $ ">> " ++ printTree i3 ++ ":" ++ show idx ++ "()"

  subl (LImm argOffset) esp

  mapM_ computeArg $ zip [0..] args'
  popl  eax
  pushl eax
  subl  (LImm 4) eax
  icall (LRel EAX (PointerOffset (varSize * idx)))
  blankLine

  addl (LImm argOffset) esp
  comment $ "<< " ++ printTree i3 ++ ":" ++ show idx ++ "()"

  pushl eax
  where
    args' = e2 : args
    argOffset = length args' * varSize
    computeArg (n, e1) = do
      let argloc = LRel ESP $ PointerOffset (n * varSize)
      emitExpr e1
      popl eax
      movl eax argloc

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
    Plus_Int ->        addl  ecx eax  >> pushl eax
    Minus    ->        subl  ecx eax  >> pushl eax
    Times    ->        imull ecx eax  >> pushl eax
    Div      -> cdq >> idivl ecx      >> pushl eax
    Mod      -> cdq >> idivl ecx      >> pushl edx
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
    EQU_Arr  -> referenceEquality
    NE_Arr   -> referenceNEquality
    EQU_Ref  -> referenceEquality
    NE_Ref   -> referenceNEquality
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
      genJump' je  jne
    referenceNEquality :: X86M ()
    referenceNEquality = do
      loadValues
      cmpl    eax ecx
      genJump' jne je
    comparison :: WriteInstr1A -> WriteInstr1A -> X86M ()
    comparison posJ negJ = do
      loadValues
      cmpl    ecx eax
      genJump' posJ negJ
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
    genJump' :: WriteInstr1A -> WriteInstr1A -> X86M ()
    genJump' = genJump lTrue lFalse lNext

emitBooleanExpr (Not e1) lTrue lFalse lNext =
  emitBooleanExpr e1 lFalse lTrue lNext

emitBooleanExpr (ELitTrue) lTrue _ lNext =
  if lTrue  == lNext then return () else jmp (LLbl lTrue)

emitBooleanExpr (ELitFalse) _ lFalse lNext =
  if lFalse == lNext then return () else jmp (LLbl lFalse)

-- Other boolean expression, evaluate it and get the result from stack
emitBooleanExpr e1 lTrue lFalse lNext = do
  emitExpr e1
  popl eax
  test eax eax
  genJump lTrue lFalse lNext jne je

genJump :: Label -> Label -> Label -> WriteInstr1A -> WriteInstr1A -> X86M ()
genJump lTrue lFalse lNext posJump negJump
  | lNext == lTrue  = negJump (LLbl lFalse)
  | lNext == lFalse = posJump (LLbl lTrue )
  | otherwise       = posJump (LLbl lTrue ) >> jmp (LLbl lFalse)

-----------------------
-- VTable generation --
-----------------------

emitVtables :: X86M ()
emitVtables = do
  tenv <- use env
  let classes = tenv ^. _3
  mapM_ emitVtable (M.elems classes)

emitVtable :: Class -> X86M ()
emitVtable cls = do
  let methods = M.elems $ allMethods cls
  let pairs   = L.sortOn snd [ (labelPrefix' (getIdent f) (Just origClsId), idx) | Method f idx origClsId <- methods ]
  let label@(Label lblstr) = getVtableLabel' (getIdent cls)
  let aHead  = [ SDirective $ DArrayHeader lblstr (length pairs * varSize), SLabel label ]
  let elems  = map (\(l,_) -> SDirective $ DLong $ Label l) pairs
  let stmts  = reverse (aHead ++ elems)

  emittedStmts %= (stmts++)

  liftIO $ putStrLn $ show $ getIdent cls
  liftIO $ putStrLn $ show pairs






