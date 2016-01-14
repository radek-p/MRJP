{-# LANGUAGE GADTs #-}
module Frontend.SemanticAnalysis.Checks.IntegersInBounds where

import Data.Int
import Control.Monad

import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.Monad
import Frontend.SemanticAnalysis.CheckError


checkIIB :: Tree a -> CheckM ()
checkIIB x@(ELitInt n) = CEContext x $$ do
  unless (n >= (fromIntegral (minBound :: Int32)) && n <= (fromIntegral (maxBound :: Int32))) $
    throwCheckError (IntegerOutOfBounds n)
checkIIB x = composOpM_ checkIIB x