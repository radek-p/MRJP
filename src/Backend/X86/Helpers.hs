{-# LANGUAGE GADTs #-}
module Backend.X86.Helpers where

import Control.Monad.Identity
import Control.Monad.State
import Control.Lens
import qualified Data.Map as M

import Language.BuiltIns
import Frontend.Parser.AbsLatte
import Backend.X86.DataTypes

-----------------------------------------------------
-- Calculate needed stack size for local variables --
-----------------------------------------------------

-- For the sake of simplicity each datatype takes 4
-- bytes of memory (even boolean value).

varSize :: Int
varSize = 4

-- Important assumption: variable idents *must* be
-- unique in function definition Tree
type AssignState = (LocalOffsetEnv, PointerOffset, Bool)
type AssignM = StateT AssignState Identity ()

offEnv :: Lens' AssignState LocalOffsetEnv
offEnv = _1

curOff :: Lens' AssignState PointerOffset
curOff = _2

inClass :: Lens' AssignState Bool
inClass = _3

declArg :: (Ident, PointerOffset) -> AssignM
declArg (ident, offset) =
  offEnv %= M.insert ident offset

newScope :: AssignM -> AssignM
newScope m = do
  offset <- use curOff
  m
  curOff .= offset

initialVarOffset :: PointerOffset
initialVarOffset = PointerOffset $ 0 -- this offset is first decreased by size of the variable

declVar :: Ident -> AssignM
declVar ident = do
  PointerOffset offset <- use curOff
  let newOffset = PointerOffset (offset - varSize)
  offEnv %= M.insert ident newOffset
  curOff .= newOffset

getFrameOffsets :: Bool -> FnDef -> LocalOffsetEnv
getFrameOffsets inC x = runIdentity (execStateT (assignFrameOffsets x) (M.empty, initialVarOffset, inC)) ^. offEnv

assignFrameOffsets :: Tree a -> AssignM
assignFrameOffsets x = case x of
  FnDef _ _ args _ -> do
    inC <- use inClass
    let args' = (if inC then [thisIdent] else []) ++ [ ident | Arg _ ident <- args]
    mapM_ declArg (zip args' (map PointerOffset [8,8+varSize..]))
    newScope $ composOpM_ assignFrameOffsets x
  Init ident _ ->
    declVar ident
  NoInit ident ->
    declVar ident
  BStmt _ ->
    newScope $ composOpM_ assignFrameOffsets x
  _ -> composOpM_ assignFrameOffsets x

minOffset :: PointerOffset -> PointerOffset -> PointerOffset
minOffset (PointerOffset o1) (PointerOffset o2) = PointerOffset (min o1 o2)

getStackOffset :: X86M Loc
getStackOffset = do
  PointerOffset off <- uses localOffsetEnv (M.fold minOffset initialVarOffset)
  return $ LImm (-off)
