{-# LANGUAGE GADTs #-}
module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad.Except
import Control.Monad.State

import Frontend.SemanticAnalysis.Runner
import Frontend.Parser.ParLatte
import Frontend.Parser.PrintLatte
import Frontend.Parser.AbsLatte
import Frontend.Parser.ErrM


type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = putStrLn f >> readFile f >>= run v

run :: Verbosity -> String -> IO ()
run _ programText = do
  program <- parseProgram programText
  compileProgram program
  exitSuccess

parseProgram :: String -> IO Program
parseProgram programText = do
  let tokens = myLexer programText
  let parsingResult = pProgram tokens
  case parsingResult of
    Bad s     -> putStrLn "\n[FAIL] Parsing. Error: " >> putStrLn s >> exitFailure
    Ok result -> return result

compileProgram :: Program -> IO ()
compileProgram tree = do
  putStrLn "\n[ OK ] Parsing."
  putStrLn $ "Parsed:\n" ++ printTree tree
  checkRes <-  runExceptT (evalStateT (checkProgram tree) ())
  case checkRes of
    Left err -> print err >> exitFailure
    Right () -> putStrLn "[ OK ] Program checked."

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
    [] -> getContents >>= run 2
    "-s":fs -> mapM_ (runFile 0) fs
    fs -> mapM_ (runFile 2) fs





