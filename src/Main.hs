{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}
module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import Frontend.StaticChecks
import Frontend.Preprocessing
import Frontend.Utility

import Syntax.LexLatte
import Syntax.ParLatte
import Syntax.SkelLatte
import Syntax.PrintLatte
import Syntax.AbsLatte
import Syntax.ErrM

import Data.Monoid
import Control.Monad.Error
import Control.Monad.State

type ParseFun = [Token] -> Err Program
type Verbosity = Int

myLLexer = myLexer

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: Verbosity -> ParseFun -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\n[FAIL] Parsing."
                          putStrLn "Error:"
                          putStrLn s
                          exitFailure
           Ok  tree@(Program _) -> do
                          putStrLn "\n[ OK ] Parsing."
                          let tree' = preprocessProgram tree
                          putStrLn $ "After preprocessing:\n" ++ printTree tree'
                          checkRes <-  runErrorT (evalStateT (checkProgram tree') ())
                          case checkRes of
                            Left err -> print err >> exitFailure
                            Right () -> putStrLn "[ OK ] Program checked."
                          exitSuccess
           _ -> putStrLn "Wrong type." >> exitFailure

showTree :: Int -> Program -> IO ()
showTree v tree = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> hGetContents stdin >>= run 2 pProgram
    "-s":fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (runFile 2 pProgram) fs





