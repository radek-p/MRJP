{-# LANGUAGE GADTs, KindSignatures, Rank2Types, DataKinds, PolyKinds, FlexibleContexts #-}
module Frontend.SemanticAnalysis.Runner where

import Prelude hiding (cycle)
import Control.Lens

import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.Monad

import Frontend.SemanticAnalysis.Checks.CyclicInherritance
import Frontend.SemanticAnalysis.Checks.UniqueIdents
import Frontend.SemanticAnalysis.ContextUpdates.BuildEnv
import Frontend.SemanticAnalysis.Checks.FieldsNotInitialised


checkProgram :: Program -> CheckM ()
checkProgram p = do
  -- first pass of checks
  mapM_ (\c -> c p) [ checkCI, checkIdentsUnique, checkFNI ]

  -- bulding type environment for second pass of checks
  env <~ buildEnv p

  -- second pass of checks
  return ()


--collectClasses :: Program -> CheckM' () [Class]
--collectClasses (Program topdefs) =
--  let
--    trees          = G.components g
--    topClassVertex = MB.fromJust $ k2v objectClassIdent -- TODO do not use fromJust
--  in do
--    case trees of
--      []   -> error "This list shall never be empty"
--      [_] -> do
--        let [tree] = G.dfs (G.transposeG g) [topClassVertex]
--        let clsTree = fmap getCls tree
--        return $ T.flatten (updateCls clsTree)
--      _    -> do
--        let cycles  = [ lst   | lst <- map T.flatten trees, topClassVertex `notElem` lst ]
--        let classes = [ [ ident | (_, ident, _) <- map v2n cycle ] | cycle <- cycles ]
--        throwCheckError (CyclicInherritance classes)
--      where
--        getVEnv :: [MemberDef] -> Env' Variable
--        getVEnv members =
--          M.fromList [ (ident, Variable t ident) |
--                       (FieldDef (VarDef t items)) <- members,
--                       (NoInit ident)              <- items    ]
--        getFEnv :: [MemberDef] -> Env' Function
--        getFEnv members =
--          M.fromList [ (ident, Function t ident [Variable typ i | (Arg typ i) <- args] body) |
--                        MetDef (FnDef t ident args body) <- members ]
--        topClass = [ (Object, objectClassIdent, []) ]
--        edges = [ (SubClass name Object (getVEnv members) (getFEnv members), name, [supName]) |
--                  ClsTopDef (ClsDefEx name supName members) <- topdefs ] ++ topClass
--        (g, v2n, k2v) = G.graphFromEdges edges
--        getCls v = let (c, _, _) = v2n v in c
--        updateCls :: T.Tree Class -> T.Tree Class
--        updateCls (T.Node super subs) =
--          let
--            children = map (\x -> case x of
--                T.Node (SubClass a _ b c) sc -> T.Node (SubClass a super b c) sc
--                _                            -> error "Internal error"
--              ) subs
--          in
--            T.Node super (map updateCls children)


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

