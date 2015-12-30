{-# LANGUAGE GADTs, KindSignatures, Rank2Types, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Language.BuiltIns where

import Control.Lens
import qualified Data.Map as M

import Frontend.Parser.AbsLatte


objectClassIdent :: Ident
objectClassIdent = Ident "_Object"

mainClassIdent :: Ident
mainClassIdent = Ident "_Main"

-- Built in types
tInt, tBool, tString, tVoid :: Type
[tInt, tBool, tString, tVoid] = map (SimpleT . Ident) ["int", "boolean", "string", "void"]

defaultValue :: Type -> Expr
defaultValue t
  | t == tInt    = ELitInt 0
  | t == tBool   = ELitFalse
  | t == tString = EString ""
  | t == tVoid   = error "Assert: Tried to access default value of void type"
  | otherwise    = ELitNull t


data Variable
  = Variable Type Ident
  deriving Show

data Function
  = Function  Type Ident [Variable] Block
  | BuiltInFn Type Ident
  deriving Show

data Class
  = SubClass Ident           -- name
             Class           -- superclass
             (Env' Variable) -- fields
             (Env' Function) -- methods
  | Object                   -- superclass of all classes
  deriving Show


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
  getType = SimpleT . getIdent

type Env' a = M.Map Ident a
type Env = (Env' Variable, Env' Function, Env' Class)


vEnv :: Lens' Env (Env' Variable)
fEnv :: Lens' Env (Env' Function)
cEnv :: Lens' Env (Env'    Class)
vEnv = lens (\(ve,  _,  _) -> ve) (\( _, fe, ce) ve -> (ve, fe, ce))
fEnv = lens (\( _, fe,  _) -> fe) (\(ve,  _, ce) fe -> (ve, fe, ce))
cEnv = lens (\( _,  _, ce) -> ce) (\(ve, fe,  _) ce -> (ve, fe, ce))