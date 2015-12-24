{-# LANGUAGE GADTs #-}
module Frontend.Utility.PrettyPrinting where

import qualified Syntax.PrintLatte as P
import Syntax.AbsLatte


class Printable a where
  printTree :: a -> String

instance Printable (Tree a) where
  printTree x = case x of
    Program   {} -> P.printTree x
    FnTopDef  {} -> P.printTree x
    ClsTopDef {} -> P.printTree x
    MetDef    {} -> P.printTree x
    FieldDef  {} -> P.printTree x
    Arg       {} -> P.printTree x
    FnDef     {} -> P.printTree x
    ClsDef    {} -> P.printTree x
    ClsDefEx  {} -> P.printTree x
    VarDef    {} -> P.printTree x
    Block     {} -> P.printTree x
    Empty     {} -> P.printTree x
    BStmt     {} -> P.printTree x
    Decl      {} -> P.printTree x
    Ass       {} -> P.printTree x
    Incr      {} -> P.printTree x
    Decr      {} -> P.printTree x
    Ret       {} -> P.printTree x
    VRet      {} -> P.printTree x
    Cond      {} -> P.printTree x
    CondElse  {} -> P.printTree x
    While     {} -> P.printTree x
    For       {} -> P.printTree x
    SExp      {} -> P.printTree x
    NoInit    {} -> P.printTree x
    Init      {} -> P.printTree x
    SimpleT   {} -> P.printTree x
    ArrayT    {} -> P.printTree x
    FunT      {} -> P.printTree x
    EVar      {} -> P.printTree x
    ELitInt   {} -> P.printTree x
    ELitTrue  {} -> P.printTree x
    ELitFalse {} -> P.printTree x
    ELitNull  {} -> P.printTree x
    EApp      {} -> P.printTree x
    EString   {} -> P.printTree x
    ArrAccess {} -> P.printTree x
    ClsAccess {} -> P.printTree x
    ClsApply  {} -> P.printTree x
    ArrAlloc  {} -> P.printTree x
    ClsAlloc  {} -> P.printTree x
    Neg       {} -> P.printTree x
    Not       {} -> P.printTree x
    EMul      {} -> P.printTree x
    EAdd      {} -> P.printTree x
    ERel      {} -> P.printTree x
    EAnd      {} -> P.printTree x
    EOr       {} -> P.printTree x
    LVar      {} -> P.printTree x
    LArrAcc   {} -> P.printTree x
    LClsAcc   {} -> P.printTree x
    Plus      {} -> P.printTree x
    Minus     {} -> P.printTree x
    Times     {} -> P.printTree x
    Div       {} -> P.printTree x
    Mod       {} -> P.printTree x
    LTH       {} -> P.printTree x
    LE        {} -> P.printTree x
    GTH       {} -> P.printTree x
    GE        {} -> P.printTree x
    EQU       {} -> P.printTree x
    NE        {} -> P.printTree x
    Ident     {} -> P.printTree x
