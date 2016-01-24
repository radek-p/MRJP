{-# LANGUAGE GADTs, KindSignatures, Rank2Types, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Language.BuiltIns where

import qualified Data.Map as M

import Frontend.Parser.AbsLatte


objectClassIdent :: Ident
objectClassIdent = Ident "_Object"

-- Built in types
builtInIdents :: [Ident]
builtInIdents = map Ident ["int", "boolean", "string", "void"]

lengthIdent :: Ident
lengthIdent = Ident "length"

thisIdent :: Ident
thisIdent = Ident "this"

isArrayType :: Type -> Bool
isArrayType (ArrayT _) = True
isArrayType _          = False

isObjectType :: Type -> Bool
isObjectType (ClassT _) = True
isObjectType _          = False

data Variable
  = Variable Type Ident
  deriving (Show, Eq)

data Function
  = Function  Type Ident [Variable] Block
  | BuiltInFn Type Ident
  deriving (Show, Eq)

data Class
  = SubClass Ident           -- name
             Class           -- superclass
             (Env' Variable) -- fields
             (Env' Function) -- methods
  | Object                   -- superclass of all classes
  deriving (Show, Eq)


class HasIdent a where
  getIdent :: a -> Ident
  getIdentStr :: a -> String
  getIdentStr x = let (Ident str) = getIdent x in str

instance HasIdent Ident where
  getIdent = id

instance HasIdent Variable where
  getIdent (Variable _ i) = i

instance HasIdent Function where
  getIdent (Function  _ i _ _) = i
  getIdent (BuiltInFn _ i    ) = i

instance HasIdent Class where
  getIdent (SubClass i _ _ _) = i
  getIdent (Object)           = objectClassIdent


class HasType a where
  getType :: a -> Type

instance HasType Variable where
  getType (Variable t _) = t

instance HasType Function where
  getType (Function t _ _ _) = t
  getType (BuiltInFn  t _  ) = t

instance HasType Class where
  getType = ClassT . getIdent


type Env' a = M.Map Ident a
type Env = (Env' Variable, Env' Function, Env' Class)

concatIdent :: Ident
concatIdent = Ident "liblatteConcat"

builtInFunctions :: [Function]
builtInFunctions = [
    BuiltInFn (FunT VoidT   [IntT]   ) (Ident "printInt"),
    BuiltInFn (FunT VoidT   [StringT]) (Ident "printString"),
    BuiltInFn (FunT IntT    []       ) (Ident "readInt"),
    BuiltInFn (FunT StringT []       ) (Ident "readString"),
    BuiltInFn (FunT VoidT   []       ) (Ident "error"),
    BuiltInFn (FunT StringT [StringT, StringT]) concatIdent
  ]

defaultValue :: Type -> Expr
defaultValue t = case t of
  IntT     -> ELitInt     0
  BooleanT -> ELitFalse
  StringT  -> EString  ""
  VoidT    -> error "Void has no default value"
  _        -> ELitNull t

initialFEnv :: Env' Function
initialFEnv = M.fromList [
    (getIdent fn, fn) | fn <- builtInFunctions
  ]

initialEnv :: Env
initialEnv = (M.empty, initialFEnv, M.empty)