{-# LANGUAGE GADTs #-}
module Utility.PrettyPrinting where

import qualified Frontend.Parser.PrintLatte as P
import Frontend.Parser.AbsLatte
import Data.List ( intercalate )


lines' :: String -> [String]
lines' [] = [[]]
lines' s = case span (/='\n') s of
  (sh, _:st) -> sh : (lines' st)
  (sh,   []) -> [ sh ]

unlines' :: [String] -> String
unlines' = intercalate "\n"

prefixLines :: String -> String -> String
prefixLines pref str = unlines' [ pref ++ s | s <- lines' str ]

indent :: Int -> String -> String
indent n = prefixLines [' ' | _ <- [1..n]]

printColorSingle :: String -> String -> String
printColorSingle ccode s = ccode ++ s ++ "\x1b[0m"

printColor :: String -> String -> String
printColor ccode s = unlines' [ printColorSingle ccode l | l <- lines' s ]

printRed :: String -> String
printRed = printColor "\x1b[31m"

printGreen :: String -> String
printGreen = printColor "\x1b[32m"

printWhite :: String -> String
printWhite = printColor "\x1b[37m"

printBoldWhite :: String -> String
printBoldWhite = printColor "\x1b[37;1m"

printBlue :: String -> String
printBlue = printColor "\x1b[34m"

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
    ClassT    {} -> P.printTree x
    ArrayT    {} -> P.printTree x
    FunT      {} -> P.printTree x
    ELitInt   {} -> P.printTree x
    ELitTrue  {} -> P.printTree x
    ELitFalse {} -> P.printTree x
    ELitNull  {} -> P.printTree x
    EApp      {} -> P.printTree x
    EString   {} -> P.printTree x
    ELVal     {} -> P.printTree x
    -- EVar      {} -> P.printTree x
    -- ArrAccess {} -> P.printTree x
    -- ClsAccess {} -> P.printTree x
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
    EBinOp    {} -> P.printTree x
    AND       {} -> P.printTree x
    OR        {} -> P.printTree x
    EQU_Int   {} -> P.printTree x
    EQU_Bool  {} -> P.printTree x
    EQU_Str   {} -> P.printTree x
    EQU_Arr   {} -> P.printTree x
    EQU_Ref   {} -> P.printTree x
    Plus_Str  {} -> P.printTree x
    Plus_Int  {} -> P.printTree x
    IntT      {} -> P.printTree x
    StringT   {} -> P.printTree x
    BooleanT  {} -> P.printTree x
    VoidT     {} -> P.printTree x
    FdNoInit  {} -> P.printTree x
    NE_Int    {} -> P.printTree x
    NE_Str    {} -> P.printTree x
    NE_Arr    {} -> P.printTree x
    NE_Ref    {} -> P.printTree x
    NE_Bool   {} -> P.printTree x
