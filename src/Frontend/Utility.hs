{-# LANGUAGE GADTs, KindSignatures, Rank2Types, DataKinds, PolyKinds, FlexibleContexts #-}

module Frontend.Utility where

import Syntax.LexLatte
import Syntax.ParLatte
import Syntax.SkelLatte
import Syntax.PrintLatte
import Syntax.AbsLatte

import Control.Monad.Identity
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error

import System.IO


walkAround :: Int -> Tree a -> IO ()
walkAround n = composOpM_ processNode
  where
    prefix :: String
    prefix = pref $ 2 * n
    processNode :: forall a. Tree a -> IO ()
    processNode x = do
      let ctr = unwords (take 5 (words (printTree x)))

      putStr (prefix ++ "--> " ++ ctr)
      hFlush stdout

      line <- getLine
      putStr (case line of { h:t -> " "; [] -> "." })

      walkAround (n+1) x

      putStr (prefix ++ "<-- " ++ ctr)
      hFlush stdout

      line <- getLine
      putStr (case line of { h:t -> " "; [] -> "."})

pref :: Int -> String
pref n = [ ' ' | i <- [1..n] ]

--wrapWalkAround :: (Tree a -> m IO a) -> Tree a -> m IO a
--wrapWalkAround = wrapWalkAround' 0
--
--wrapWalkAround' :: Int -> (Tree a -> m IO a) -> Tree a -> m IO ()
--wrapWalkAround' n mf t = composOpM_ processNode t
--  where
--    prefix :: String
--    prefix = pref $ 2*n
--    processNode :: forall a. Tree a -> IO ()
--    processNode x = do
--      let ctr = unwords (take 5 (words (printTree x)))
--
--      liftIO $ putStr (prefix ++ "--> " ++ ctr)
--      liftIO $ hFlush stdout
--
--      line <- liftIO getLine
--      liftIO $ putStr (case line of { h:t -> " "; [] -> "." })
--
--      walkAround (n+1) x
--
--      liftIO $ putStr (prefix ++ "<-- " ++ ctr)
--      liftIO $ hFlush stdout
--
--      line <- liftIO getLine
--      liftIO $ putStr (case line of { h:t -> " "; [] -> "."})

