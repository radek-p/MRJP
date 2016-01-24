{-# LANGUAGE GADTs #-}
module Backend.X86.Transformations.DesugaringOfForLoops where

import Language.BuiltIns
import Frontend.Parser.AbsLatte


-------------------------------------------------------------------
-- Initialize all uninitialized variables with the default value --
-------------------------------------------------------------------

desugarForLoops :: Tree a -> Tree a
desugarForLoops = composOp processNode
  where
    processNode :: Tree a -> Tree a
    processNode x = case x of
      For t1 i2 e3 s4 -> BStmt $ Block [
          -- int #For_counter = 0 ;
          Decl IntT [Init counterId (ELitInt 0)],
          -- t1[] #For_array = e3 ;
          Decl (ArrayT t1) [Init arrayId e3],
          -- t1  i2 ;
          Decl t1 [NoInit i2],
          -- while (#For_counter < #For_array.length) {
          While (EBinOp (ELVal $ LVar counterId) LTH (ELVal $ LTClsAcc (ArrayT t1) (ELVal (LVar arrayId)) lengthIdent)) $ BStmt (Block [
              -- s4 ;
              desugarForLoops s4,
              -- #For_counter++;
              Incr (LVar counterId)
            ])
          -- }
        ]
        where
          counterId = Ident "#For_counter"
          arrayId   = Ident "#For_array"
      _            -> desugarForLoops x