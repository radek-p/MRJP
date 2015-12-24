{-# LANGUAGE GADTs, KindSignatures, Rank2Types, DataKinds, PolyKinds, FlexibleContexts #-}
module Frontend.Preprocessing.Runner where

import Syntax.AbsLatte
import Language.BuiltIns


preprocessProgram :: Program -> Program
preprocessProgram p =
  foldl (\x op -> op x) p [
      deriveAllClassesFromObject,
      initializeAllVariables
    ]

deriveAllClassesFromObject :: Tree a -> Tree a
deriveAllClassesFromObject = composOp processNode
  where
    processNode :: forall a. Tree a -> Tree a
    processNode x = case x of
      ClsDef ident members -> ClsDefEx ident objectClassIdent members
      _                    -> deriveAllClassesFromObject x

initializeAllVariables :: Tree a -> Tree a
initializeAllVariables = composOp processNode
  where
    processNode :: forall a. Tree a -> Tree a
    processNode x = case x of
      VarDef t items -> VarDef t (initialize t items)
      ClsDefEx a b members ->
        ClsDefEx a b (map
          (\x' -> case x' of { FieldDef _ -> x'; _ -> initializeAllVariables x' })
          members)
      _              -> initializeAllVariables x
    initialize :: Type -> [Item] -> [Item]
    initialize t =
      let val = defaultValue t in
      map (\x -> case x of { Init _ _ -> x; NoInit ident -> Init ident val })
