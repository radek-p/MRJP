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
thisIdent = Ident "self"

isArrayType :: Type -> Bool
isArrayType (ArrayT _) = True
isArrayType _          = False

isObjectType :: Type -> Bool
isObjectType (ClassT _) = True
isObjectType _          = False

data Variable
  = Variable Type Ident
  deriving (Show, Eq)

data ClassField
  = ClassField Variable Int
  deriving (Show, Eq)

data Function
  = Function  Type Ident
  deriving (Show, Eq)

data Class
  = SubClass Ident             -- name
             Class             -- superclass
             Int               -- number of fields (including those defined in superclasses)
             (Env' ClassField) -- fields
             (Env' Function)   -- methods
  | Object                     -- superclass of all classes
  deriving (Show, Eq)

class HasSize a where
  getSize :: a -> Int

instance HasSize Class where
  getSize Object = 0
  getSize (SubClass _ _ sz _ _) = sz

class HasIdent a where
  getIdent :: a -> Ident
  getIdentStr :: a -> String
  getIdentStr x = let (Ident str) = getIdent x in str

instance HasIdent Ident where
  getIdent = id

instance HasIdent Variable where
  getIdent (Variable _ i) = i

instance HasIdent Function where
  getIdent (Function  _ i) = i

instance HasIdent Class where
  getIdent (SubClass i _ _ _ _) = i
  getIdent (Object)           = objectClassIdent

instance HasIdent ClassField where
  getIdent (ClassField v _) = getIdent v


class HasType a where
  getType :: a -> Type

instance HasType Variable where
  getType (Variable t _) = t

instance HasType Function where
  getType (Function t _) = t

instance HasType Class where
  getType = ClassT . getIdent

instance HasType ClassField where
  getType (ClassField v _) = getType v

type Env' a = M.Map Ident a
type Env = (Env' Variable, Env' Function, Env' Class)

concatIdent :: Ident
concatIdent = Ident "liblatteConcat"

builtInFunctions :: [Function]
builtInFunctions = [
    Function (FunT VoidT   [IntT]   ) (Ident "printInt"),
    Function (FunT VoidT   [StringT]) (Ident "printString"),
    Function (FunT IntT    []       ) (Ident "readInt"),
    Function (FunT StringT []       ) (Ident "readString"),
    Function (FunT VoidT   []       ) (Ident "error"),
    Function (FunT StringT [StringT, StringT]) concatIdent
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