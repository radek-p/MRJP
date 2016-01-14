{-# LANGUAGE GADTs, KindSignatures, Rank2Types #-}
module Frontend.SemanticAnalysis.CheckError where

import Data.List ( intercalate )

import Frontend.Parser.AbsLatte
import Utility.PrettyPrinting
import Language.BuiltIns


data CEType
  = OtherException String -- required by Error class
  | FunctionNamesNotUnique
  | ClassNamesNotUnique
  | MainFunctionNotDefined
  | RestrictedIdentifier Ident
  | UninitializedVarUsage Ident
  | CyclicInherritance [[Ident]]
  | ClassFieldInitialised
  | NestedVariableDeclaration
  | TypeError TypeError
  | FeatureNotSupported String
  | MissingReturnStatement Ident
  | IntegerOutOfBounds     Integer
  | RedefinitionOfBuiltInFunctions [Ident]

data TypeError
  = ClassNotFound    Ident
  | FunctionNotFound Ident
  | VariableNotFound Ident
  | MethodNotFound   Ident Class
  | FieldNotFound    Ident Class
  | FieldOfString    Ident
  | FieldOfArray     Ident
  | FieldOfFunction  Ident
  | FieldOfBuiltIn   Type
  | InvalidTypeOfNullLit Type
  | TEApp [Type] [Type]
  | TEArrIdx     Type
  | TEArrAlocLen Type
  | TEArr        Type
  | IncompatibleTypes Type Type
  | ObjAllocBadType  Type
  | InvalidOperandTypes Type Type
  | InvalidNumberOfArguments [Type] [Type]
  | IdentifierAlreadyDefined Ident
  deriving Show

data CEContext
  = forall a. CEContext (Tree a)

data CheckError
  = CheckError CEType [CEContext]


instance Show CEType where
  show err = case err of
    OtherException s           -> "Other error: " ++ s
    FunctionNamesNotUnique     -> "Function names are not unique"
    ClassNamesNotUnique        -> "Class names are not unique"
    MainFunctionNotDefined     -> "There is not definition of required function  int main()"
    UninitializedVarUsage i    -> "Variable " ++ printTree i ++ "  was not initialized before use" -- TODO Maybe remove.
    CyclicInherritance ids     -> "Cyclic inherritance detected:  " ++
                                  intercalate ";" [ intercalate "," (map printTree cyc) | cyc <- ids ]
    ClassFieldInitialised      -> "Class fields cannot be initialised with default value."
    NestedVariableDeclaration  -> "Variables can be declared only at block level."
    RestrictedIdentifier i     -> "Restricted identifier  " ++ printTree i ++ "  was used."
    TypeError e                -> "Type error: " ++ show e
    FeatureNotSupported s      -> "Feature is not supported yet: " ++ s
    MissingReturnStatement _   -> "Missing return statement."
    IntegerOutOfBounds n       -> "Integer literal  " ++ show n ++ "  is out of bounds."
    RedefinitionOfBuiltInFunctions lst
                               -> "Redefinition of built in function(s): " ++ concat [ printTree i ++ ", " | i <- lst ]

showFnDef :: FnDef -> String
showFnDef (FnDef typ ident args _) =
  printTree typ ++ " " ++ printTree ident ++ "(" ++ foldr ((++).printTree) "" args ++ ")"

showClsDef :: Ident -> Ident -> String
showClsDef ident super = unlines [
    printWhite "In definition of class  " ++ printBoldWhite (printTree ident) ++ ":",
    "   " ++ printTree ident ++ " extends " ++ printTree super
  ]

instance Show CEContext where
  show (CEContext x) = case x of
    FnTopDef def@(FnDef _ ident _ _) -> unlines [
        printWhite "In definition of function  " ++ printBoldWhite (printTree ident) ++ printWhite "  with signature:",
        "   " ++ showFnDef def
      ]
    ClsDef   ident _       -> showClsDef ident objectClassIdent
    ClsDefEx ident super _ -> showClsDef ident super
    Init i _ -> unlines [
        printWhite "In definition of variable  " ++ printBoldWhite (printTree i) ++ printWhite "  :",
        "   " ++ printTree x
      ]
    EApp i _ -> unlines [
        printWhite "In call of function  " ++ printBoldWhite (printTree i) ++ printWhite "  :",
        "   " ++ printTree x
      ]
    Decl _ _ -> unlines [
        printWhite "In declaration of variable(s):",
        "   " ++ printTree x
      ]
    Cond{}     -> unlines [ printWhite "In  " ++ printBoldWhite "if"      ++ printWhite "  statement." ]
    CondElse{} -> unlines [ printWhite "In  " ++ printBoldWhite "if else" ++ printWhite "  statement." ]
    FnDef{} -> ""
    Block{} -> ""
    _ -> words (show x) !! 0 ++ "\n   " ++ printTree x ++ "\n"

printContexts :: [CEContext] -> String
printContexts ctxs =
  let cstrs = filter (/="") [ show ctx | ctx <- reverse ctxs ] in
  concat [ show i ++ ". " ++ str | (i, str) <- zip [(1::Integer)..] cstrs ]

instance Show CheckError where
  show (CheckError typ ctx) = unlines [
      printRed $ "Error: " ++ show typ,
      "Context:",
      unlines' [ "   " ++ line | line <- lines $ printContexts ctx, line /= "" ]
    ]
