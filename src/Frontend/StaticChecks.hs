{-# LANGUAGE GADTs, KindSignatures, Rank2Types, DataKinds, PolyKinds, FlexibleContexts #-}
module Frontend.StaticChecks where

import Syntax.LexLatte
import Syntax.ParLatte
import Syntax.SkelLatte
import Syntax.PrintLatte
import Syntax.AbsLatte

import Control.Monad.Identity
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Error

import Data.Monoid
import qualified Data.Set as S

-- Testy do wykonania:
--   - czy istnieje funkcja main,
--   - czy nazwy funkcji są jednoznaczne,
--   - czy nazwy zmiennych są jednoznaczne,
--   - czy zmienne zadeklarowane przed użyciem,
--   - czy wyrażenia mają poprawny typ,

data CheckException
  = OtherException String
  | FunctionNamesNotUnique
  | MainFunctionNotDefined
  | UninitializedVarUsage Ident
    deriving Show

instance Error CheckException where
  strMsg = OtherException
  noMsg  = strMsg ""

--type CheckMonad' r a = ReaderT r (ErrorT CheckException IO) a
type CheckMonad' r a = ErrorT CheckException (ReaderT r IO) a
type CheckMonad      = CheckMonad' () ()

checkProgram :: Program -> CheckMonad
checkProgram p =
  mapM_ (\c -> c p) [
      checkFunctionNames
    ]

checkFunctionNames :: Program -> CheckMonad
checkFunctionNames (Program topdefs)
  | length idents /= S.size idents' = throwError FunctionNamesNotUnique
  | not $ S.member "main" idents'   = throwError MainFunctionNotDefined
  | otherwise                       = return ()
    where
      idents  = [ ident | FnTopDef (FnDef retTyp (Ident ident) args body) <- topdefs ]
      idents' = S.fromList idents

--checkClassNames :: Program -> CheckMonad
--checkClassNames (Program topdefs)
--  | length idents /= S.size idents' = throwError FunctionNamesNotUnique
--  | not $ S.member "main" idents'   = throwError MainFunctionNotDefined
--  | otherwise                       = return ()
--    where
--      idents  = [ ident | ClsTopDef (ClsDef retTyp (Ident ident) args body) <- topdefs ]
--      idents' = S.fromList idents

checkVarDecl :: Tree a -> CheckMonad
checkVarDecl x = (withReaderT $ const S.empty) `mapErrorT` checkVarDecl' x

--type CheckVarDeclMonad = CheckMonad' (S.Set Ident) ()
--checkVarDecl' :: Tree a -> CheckVarDeclMonad
--checkVarDecl' = composOpM_ processNode
--  where
--    processNode :: forall a. Tree a -> CheckVarDeclMonad
--    processNode x = case x of
--      Decl (VarDef _ items) -> mapErrorT (local (S.union (S.fromList [ ident | Init ident _ <- items ]))) $ checkVarDecl' x
--      FnDef _ _ args body   -> mapErrorT (local (S.union (S.fromList [ ident | Arg  _ ident <- args  ]))) $ (checkVarDecl' body >> checkVarDecl' x)
--      For _ ident _ body    -> mapErrorT (local (S.insert ident)) $ (checkVarDecl' body >> checkVarDecl' x)
--      EVar ident            -> do
--        env <- ask --(asks (S.member ident))
--        when (not $ ident `S.member` env) $ throwError (UninitializedVarUsage ident)
--      _                     -> checkVarDecl' x

topDefCount :: Program -> Int
topDefCount t = case t of
  Program topdefs -> length topdefs

sumIntLit :: (forall a. Tree a -> Int)
sumIntLit t = case t of
  ELitInt _ -> 1
  _         -> integerLiteralCount t

integerLiteralCount :: Tree a -> Int
integerLiteralCount =
  composOpFold 0 (+) sumIntLit

increaseIntegers :: Tree a -> Tree a
increaseIntegers = composOp processNode
  where
    processNode :: forall a. Tree a -> Tree a
    processNode x = case x of
      ELitInt n -> ELitInt $ n+1
      _         -> increaseIntegers x

--mts :: Tree a -> IO (Tree a)
--mts = compos return (\mf ma -> do { f <- mf; a <- ma; putStrLn "a"; return (f a) }) (\x -> do { putStrLn "<>"; (case x of {ELitInt n -> print n; _ -> return ()} ); mts x })




