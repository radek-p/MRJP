{-# LANGUAGE GADTs #-}
module Backend.X86.Helpers where

import Control.Monad.Identity
import Control.Monad.State
import Control.Lens
import qualified Data.Map as M

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
type AssignState = (LocalOffsetEnv, PointerOffset)
type AssignM = StateT AssignState Identity ()

offEnv :: Lens' AssignState LocalOffsetEnv
offEnv = lens (fst) (\(_, b) a -> (a, b))

curOff :: Lens' AssignState PointerOffset
curOff = lens (snd) (\(a, _) b -> (a, b))

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

getFrameOffsets :: FnDef -> LocalOffsetEnv
getFrameOffsets x = runIdentity (execStateT (assignFrameOffsets x) (M.empty, initialVarOffset)) ^. offEnv

assignFrameOffsets :: Tree a -> AssignM
assignFrameOffsets x = case x of
  FnDef _ _ args _ -> do
    mapM_ declArg (zip [ ident | Arg _ ident <- args] (map PointerOffset [8,8+varSize..]))
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
