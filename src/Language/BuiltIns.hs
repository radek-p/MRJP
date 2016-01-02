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

idInt, idBool, idString, idVoid :: Ident
[idInt, idBool, idString, idVoid] = builtInIdents

builtInTypes :: [Type]
builtInTypes = map (SimpleT) builtInIdents

tInt, tBool, tString, tVoid :: Type
[tInt, tBool, tString, tVoid] = builtInTypes

isBuiltIn :: Ident -> Bool
isBuiltIn i = any (==i) builtInIdents

defaultValue :: Type -> Expr
defaultValue t
  | t == tInt    = ELitInt 0
  | t == tBool   = ELitFalse
  | t == tString = EString ""
  | t == tVoid   = error "Assert: Tried to access default value of void type"
  | otherwise    = ELitNull t

-- Built in functions declarations
tI_I :: Type
tI_I = FunT tInt [tInt]
ineg :: Function
ineg = BuiltInFn tI_I (Ident "_ineg")

tI_II :: Type
tI_II = FunT tInt [tInt, tInt]
iadd, isub, imul, idiv, imod :: Function
[iadd, isub, imul, idiv, imod] = map (\suf -> BuiltInFn tI_II (Ident $ "_" ++ suf))
  ["iadd", "isub", "imul", "idiv", "imod"]

tB_II :: Type
tB_II = FunT tBool [tInt, tInt]
ilt, ile, igt, ige, ieq, ineq :: Function
[ilt, ile, igt, ige, ieq, ineq] = map (\suf -> BuiltInFn tB_II (Ident $ "_" ++ suf))
  ["ilt", "ile", "igt", "ige", "ieq", "ineq"]

tB_OO :: Type
tB_OO = FunT tBool [SimpleT objectClassIdent, SimpleT objectClassIdent]
oeq, oneq :: Function
[oeq, oneq] = map (\suf -> BuiltInFn tB_OO (Ident $ "_" ++ suf))
  ["oeq", "oneq"]

tB_B :: Type
tB_B = FunT tBool [tBool]
bnot :: Function
bnot = BuiltInFn tB_B (Ident "_bnot")

tB_BB :: Type
tB_BB = FunT tBool [tBool, tBool]
bor, band :: Function
[bor, band] = map (\suf -> BuiltInFn tB_BB (Ident $ "_" ++ suf))
  ["bor", "band"]



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
  getType = SimpleT . getIdent


type Env' a = M.Map Ident a
type Env = (Env' Variable, Env' Function, Env' Class)

--restrictedClasses :: [Class]
--restrictedClasses =
--  [ SubClass ident Object M.empty M.empty | ident <- [ idInt, idBool, idVoid ] ] ++
--  [ SubClass idString Object M.fromList [  ] ]
--
--initialClassEnv :: Env' Class
--initialClassEnv = M.fromList [
--    SubClass
--  ]