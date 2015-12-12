{-# LANGUAGE GADTs, KindSignatures, Rank2Types, DataKinds, PolyKinds, FlexibleContexts #-}
module Frontend.StaticChecks where

import Syntax.LexLatte
import Syntax.ParLatte
import Syntax.SkelLatte
import Syntax.PrintLatte
import Syntax.AbsLatte

import Control.Monad.Identity
import Control.Monad.Trans (liftIO)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error

import Data.Monoid
import qualified Data.Set as S

import System.IO

import Frontend.Utility

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
type CheckMonad' s a = StateT s (ErrorT CheckException IO) a
type CheckMonad      = CheckMonad' () ()

checkProgram :: Program -> CheckMonad
checkProgram p =
  mapM_ (\c -> c p) [
      checkFunctionNames
      , checkVarDecl
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

checkVarDecl :: Program -> CheckMonad
--checkVarDecl x = (withStateT $ const S.empty) `mapErrorT` checkVarDecl' 0 x
checkVarDecl x = StateT (\u -> do (runStateT (checkVarDecl' 0 x) S.empty) >> return ((), ()))

infixl 0 $$
($$) :: Show a => Tree l -> CheckMonad' a b -> CheckMonad' a b
x $$ m = do
  env <- get
  m `catchError` (\e -> do
    liftIO (putStrLn ("In node:\n    " ++ printTree x ++ "\n  with env: " ++ show env))
    throwError e)

type CheckVarDeclMonad = CheckMonad' (S.Set Ident) ()
checkVarDecl' :: Int -> Tree a -> CheckVarDeclMonad
checkVarDecl' n = composOpM_ processNode
  where
    m :: Int
    m = n + 1
    prefix :: String
    prefix = pref $ 2 * n
    processNodeInner :: forall a. Tree a -> CheckVarDeclMonad
    processNodeInner x@(Block stmt) = x $$ do
        env <- get
        checkVarDecl' m x
        put env
    processNodeInner x@(EVar ident) = x $$ do
        present <- gets (S.member ident)
        when (not present) $ throwError (UninitializedVarUsage ident)
    processNodeInner x = x $$ (case x of
          Decl (VarDef _ items) ->
            modify (S.union (S.fromList [ ident | Init ident _ <- items ]))
          FnDef _ _ args body   ->
            modify (S.union (S.fromList [ ident | Arg  _ ident <- args  ]))
          For _ ident _ body    ->
            modify (S.insert ident)
          _                     ->
            return ()
        ) >> checkVarDecl' m x
    processNode :: forall a. Tree a -> CheckVarDeclMonad
    processNode x = do
      let ctr = unwords (take 5 (words (printTree x)))
      let st  = prefix ++ "--> " ++ ctr
      let l   = max 1 (35 - length st)
      let st' = st ++ pref l
      env <- get
      liftIO $ putStr (st' ++ ":" ++ show env ++ "\n")
      liftIO $ hFlush stdout

      -- line <- liftIO getLine

      res <- processNodeInner x

      -- liftIO $ putStr (prefix ++ "<-- " ++ ctr)
      -- liftIO $ hFlush stdout

      -- line <- liftIO getLine
      return res

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




