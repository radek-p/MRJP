{-# LANGUAGE GADTs, KindSignatures, Rank2Types, DataKinds, PolyKinds, FlexibleContexts #-}
module Frontend.SemanticAnalysis.Runner where

import Prelude hiding (cycle)
import Control.Lens
import Control.Monad.IO.Class

import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.Monad

import Frontend.SemanticAnalysis.Checks.CyclicInherritance
import Frontend.SemanticAnalysis.Checks.UniqueIdents
import Frontend.SemanticAnalysis.ContextUpdates.BuildEnv
import Frontend.SemanticAnalysis.Checks.FieldsNotInitialised


checkProgram :: Program -> CheckM ()
checkProgram p = do
  -- first pass of checks
  liftIO $ putStr "[....] First pass of static checks."
  mapM_ (\c -> c p) [ checkCI, checkIdentsUnique, checkFNI ]
  liftIO $ putStrLn "\r[DONE] First pass of static checks."

  -- bulding type environment for second pass of checks
  liftIO $ putStr "[....] Generating environment of types."
  env <~ buildEnv p
  liftIO $ putStrLn "\r[DONE] Generating environment of types."

  -- second pass of checks
  liftIO $ putStrLn "[TODO] Second pass of static checks."
  return ()


--checkVarDecl :: Program -> CheckM ()
--checkVarDecl x = StateT (\_ -> (runStateT (checkVarDecl' 0 x) S.empty) >> return ((), ()))

--type CheckVarDeclMonad = CheckM' (S.Set Ident) ()
--checkVarDecl' :: Int -> Tree a -> CheckVarDeclMonad
--checkVarDecl' n = composOpM_ processNode
--  where
--    m :: Int
--    m = n + 1
--    prefix :: String
--    prefix = pref $ 2 * n
--    processNodeInner :: forall a. Tree a -> CheckVarDeclMonad
--    processNodeInner x@(Block _) = (CEContext x) $$ do
--        env <- get
--        checkVarDecl' m x
--        put env
--    processNodeInner x@(EVar ident) = (CEContext x) $$ do
--        present <- gets (S.member ident)
--        when (not present) $ throwCheckError (UninitializedVarUsage ident)
--    processNodeInner x = (CEContext x) $$ (case x of
--          Decl (VarDef _ items) ->
--            modify (S.union (S.fromList [ ident | Init ident _ <- items ]))
--          FnDef _ _ args _   ->
--            modify (S.union (S.fromList [ ident | Arg  _ ident <- args  ]))
--          For _ ident _ _       ->
--            modify (S.insert ident)
--          _                     ->
--            return ()
--        ) >> checkVarDecl' m x
--    processNode :: forall a. Tree a -> CheckVarDeclMonad
--    processNode x = do
--      let ctr = unwords (take 5 (words (printTree x)))
--      let st  = prefix ++ "--> " ++ ctr
--      let l   = max 1 (35 - length st)
--      let st' = st ++ pref l
--      env <- get
--      liftIO $ putStr (st' ++ ":" ++ show env ++ "\n")
--      liftIO $ hFlush stdout
--      res <- processNodeInner x
--      return res

