{-# LANGUAGE GADTs #-}
module Backend.X86.Transformations.InitialisationOfVariables where

import Language.BuiltIns
import Frontend.Parser.AbsLatte


-------------------------------------------------------------------
-- Initialize all uninitialized variables with the default value --
-------------------------------------------------------------------

initializeAllVariables :: Tree a -> Tree a
initializeAllVariables = composOp processNode
  where
    processNode :: Tree a -> Tree a
    processNode x = case x of
      Decl t items -> Decl t (initialize t items)
      _            -> initializeAllVariables x
    initialize :: Type -> [Item] -> [Item]
    initialize t =
      let val = defaultValue t in
      map (\x -> case x of { Init{} -> x; NoInit ident -> Init ident val })