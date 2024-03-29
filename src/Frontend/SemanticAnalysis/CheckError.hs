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
  | ClassFieldsNotUnique  Ident
  | ClassMethodsNotUnique Ident
  | MainFunctionNotDefined
  | RestrictedIdentifier Ident
  | CyclicInherritance [[Ident]]
  | NestedVariableDeclaration
  | TypeError TypeError
  | FeatureNotSupported String
  | MissingReturnStatement Ident
  | IntegerOutOfBounds     Integer
  | RedefinitionOfBuiltInFunctions [Ident]
  | ThisField Ident

data TypeError
  = ClassNotFound    Ident
  | FunctionNotFound Ident
  | VariableNotFound Ident
  | MethodNotFound   Ident Class
  | FieldNotFound    Ident Class
  | FieldOfArray     Ident
  | FieldOfBuiltIn   Type Ident
  | InvalidTypeOfNullLit Type
  | TEApp [Type] [Type]
  | TEArrIdx     Type
  | TEArrAlocLen Type
  | TEArr        Type
  | LengthIsNotWritable
  | IncompatibleTypes Type Type
  | ObjAllocBadType  Type
  | InvalidOperandTypes Type Type
  | InvalidNumberOfArguments [Type] [Type]
  | IdentifierAlreadyDefined Ident
  | InvalidMainSignature [Arg]
  | VoidNotAllowed
  | ArrayTypeExpected Type
  | ClassTypeExpected Type
  | CannotBeNull      Type
  | TypesNotEqual     Type Type

data CEContext
  = forall a. CEContext (Tree a)

data CheckError
  = CheckError CEType [CEContext]


instance Show CEType where
  show (TypeError t) = printRed "Type error \n" ++ show t
  show err = printRed $ case err of
    OtherException s           -> "Other error: " ++ s
    FunctionNamesNotUnique     -> "Function names are not unique"
    ClassNamesNotUnique        -> "Class names are not unique"
    MainFunctionNotDefined     -> "There is not definition of required function  " ++ printBoldWhite "int main()" ++ printRed "  ."
    CyclicInherritance ids     -> "Cyclic inherritance detected:  " ++
                                  intercalate ";" [ intercalate "," (map printTree cyc) | cyc <- ids ]
    ClassFieldsNotUnique  cls  -> "Identifiers of fields are not unique in definition of class  " ++ printBoldWhite (printTree cls) ++ printRed " ."
    ClassMethodsNotUnique cls  -> "Names of methods of class  " ++ printBoldWhite (printTree cls) ++ printRed "  are not unique."
    NestedVariableDeclaration  -> "Variables can be declared only at block level."
    RestrictedIdentifier i     -> "Restricted identifier  " ++ printTree i ++ "  was used."
    FeatureNotSupported s      -> "Feature is not supported yet: " ++ s
    MissingReturnStatement _   -> "Missing return statement."
    IntegerOutOfBounds n       -> "Integer literal  " ++ show n ++ "  is out of bounds."
    RedefinitionOfBuiltInFunctions lst
                               -> "Redefinition of built in function(s): " ++ concat [ printTree i ++ ", " | i <- lst ]
    ThisField cls              -> "Class " ++ printBoldWhite (printTree cls) ++ printRed " has a field named " ++ printBoldWhite (printTree thisIdent) ++ printRed "."
    _                          -> ""

instance Show TypeError where
  show err = printRed $ case err of
    ClassNotFound    ident -> "Unknown class usage "    ++ printTree ident
    FunctionNotFound ident -> "Unknown function usage " ++ printTree ident
    VariableNotFound ident -> "Unknown variable "       ++ printTree ident
    MethodNotFound   ident cls -> "Class " ++ printTree (getIdent cls) ++ " has no method " ++ printTree ident
    FieldNotFound    ident cls -> "Class " ++ printTree (getIdent cls) ++ " has no field "  ++ printTree ident
    FieldOfArray     ident     -> "Array doesn't have field "     ++ printTree ident
    FieldOfBuiltIn   t1 ident  -> "Builtin type " ++ printBoldWhite (printTree t1) ++ " has no field " ++ printBoldWhite (printTree ident)
    InvalidTypeOfNullLit ident -> "Invalid type of null literal " ++ printTree ident
    TEApp tl1 tl2              -> "Argument types do not match: ("
                                    ++ intercalate ", " (map printTree tl1) ++ ") /= ("
                                    ++ intercalate ", " (map printTree tl2) ++ ")"
    TEArrIdx     t             -> "Array index should have type int, but has: " ++ printTree t
    TEArrAlocLen t             -> "Allocated array length should have type int, but has: " ++ printTree t
    TEArr        t             -> "Wrong array type " ++ printTree t
    IncompatibleTypes t1 t2    -> "Incompatible types:  " ++ printBoldWhite (printTree t1) ++ printRed "  is not subtype of  " ++ printBoldWhite (printTree t2) ++ printRed "  ."
    ObjAllocBadType  t         -> "Expected class type, but got: " ++ printTree t
    InvalidOperandTypes t1 t2  -> "Incompatible operand types: " ++ printTree t1 ++ " and " ++ printTree t2
    InvalidNumberOfArguments tl1 tl2 -> "Invalid number of function arguments: got  "
                                          ++ printBoldWhite (show (length tl1)) ++ printRed "  but expected  "
                                          ++ printBoldWhite (show (length tl2)) ++ printRed "  :"
    IdentifierAlreadyDefined ident   -> "Identifier was already defined in this scope: " ++ printTree ident
    InvalidMainSignature args  -> "Invalid signature of function main: " ++ printBoldWhite ("main(" ++ intercalate ", " (map printTree args) ++ ")") ++ printRed "."
    VoidNotAllowed             -> "Void has no value, it cannot be assigned."
    LengthIsNotWritable        -> "Array property 'length' is L-Value, it cannot be assigned."
    ArrayTypeExpected t1       -> "Array type was expected, but got " ++ printBoldWhite (printTree t1) ++ printRed "."
    ClassTypeExpected t1       -> "Class type was expected, but got " ++ printBoldWhite (printTree t1) ++ printRed "."
    CannotBeNull      t1       -> "Wrong type label in (...)null literal, expected array or class type, but got " ++ printBoldWhite (printTree t1) ++ printRed "."
    TypesNotEqual     t1 t2    -> "Expected type " ++ printBoldWhite (printTree t2) ++ printRed ", but got " ++ printBoldWhite (printTree t1) ++ printRed "."

showFnDef :: FnDef -> String
showFnDef (FnDef typ ident args _) =
  printTree typ ++ " " ++ printTree ident ++ "(" ++ intercalate ", " (map printTree args) ++ ")"

showClsDef :: Ident -> Ident -> String
showClsDef ident super = unlines [
    printWhite "In definition of class  " ++ printBoldWhite (printTree ident) ++ ":",
    printTree ident ++ " extends " ++ printTree super
  ]

instance Show CEContext where
  show (CEContext x) = case x of
    FnTopDef (FnDef _ ident _ _) -> unlines [
        printWhite "In definition of function  " ++ printBoldWhite (printTree ident) ++ printWhite "  :",
        shortTree
      ]
    ClsDef   ident _       -> showClsDef ident objectClassIdent
    ClsDefEx ident super _ -> showClsDef ident super
    Init i _ -> unlines [
        printWhite "In definition of variable  " ++ printBoldWhite (printTree i) ++ printWhite "  :",
        shortTree
      ]
    EApp i _ -> unlines [
        printWhite "In call of function  " ++ printBoldWhite (printTree i) ++ printWhite "  :",
        shortTree
      ]
    Decl _ _ -> unlines [
        printWhite "In declaration of variable(s):",
        shortTree
      ]
    Cond{}      -> unlines [ printWhite "In  " ++ printBoldWhite "if"      ++ printWhite "  statement:", shortTree ]
    CondElse{}  -> unlines [ printWhite "In  " ++ printBoldWhite "if else" ++ printWhite "  statement:", shortTree ]
    ELVal ident -> unlines [ printWhite "Identifier  " ++ printBoldWhite (printTree ident) ++ printWhite "  ." ]
    Ass{}       -> unlines' [ printWhite "In assignment statement:\n" ++ shortTree ]
    VRet{}      -> unlines' [ printWhite "In return statement: \n" ++ shortTree ]
    Ret{}       -> unlines' [ printWhite "In return statement: \n" ++ shortTree]
    EBinOp{}    -> unlines [ printWhite "In binary operator expression: \n" ++ shortTree ]
    BStmt{}     -> unlines [ printWhite "In block: \n" ++ shortTree ]
    FnDef{} -> ""
    Block{} -> ""
    SExp{}  -> ""
    _ -> words (show x) !! 0 ++ "\n" ++ shortTree ++ "\n"
    where
      treeLines = filter (/="") $ lines' $ printTree x
      shortTree = printBlue ((unlines' $ take 5 treeLines) ++ if length treeLines > 5 then printWhite "\n..." else "")

formatContext :: Integer -> String -> String
formatContext i cstr =
  let allLines  = filter (/="") $ lines' cstr in
  let firstLine = unlines' (take 1 allLines) in
  let rest      = indent 3 (unlines' (drop 1 allLines)) in
  show i ++ ". " ++ firstLine ++ (if (not.null) rest then "\n" ++ rest else "")

printContexts :: [CEContext] -> String
printContexts ctxs =
  let cstrs  = filter (/="") [ show ctx | ctx <- reverse ctxs ] in
  unlines' [ formatContext i str | (i, str) <- zip [(1::Integer)..] cstrs ]

instance Show CheckError where
  show (CheckError typ ctx) =
    let contexts = indent 3 $ printContexts ctx in
    printRed "Error: " ++ show typ ++ "\n" ++
      if ((not.null) contexts) then
        "Context:\n" ++ contexts ++ "\n"
      else
        ""
