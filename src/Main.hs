{-# LANGUAGE GADTs #-}
module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.IO
import Control.Monad.Except
import Control.Monad.State

import Frontend.SemanticAnalysis.Runner
import Frontend.Parser.ParLatte
import Frontend.Parser.AbsLatte
import Frontend.Parser.ErrM
import Backend.X86.Runner


type Verbosity = Int

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> run inputFile outputFile
    [             "--help"] -> notifyUsage
    _                       -> notifyUsage

run :: FilePath -> FilePath -> IO ()
run inputFile outputFile = do
  programText <- readFile       inputFile
  programTree <- parseProgram   programText
  asmCode     <- compileProgram programTree
  writeFile outputFile asmCode
  notifySuccess

parseProgram :: String -> IO Program
parseProgram programText = do
  let tokens        = myLexer programText
  let parsingResult = pProgram tokens
  case parsingResult of
    Bad s     -> do
      putStrLn "[ FAIL] Parsing.\n Error: "
      putStrLn s
      notifyFail
    Ok result -> do
      putStrLn "[ 1/1 ] Parsing."
      return result

compileProgram :: Program -> IO String
compileProgram tree = do
  checkRes <- runExceptT (runStateT (checkProgram tree) undefined)
  (tree', _) <- case checkRes of
    Left  err -> putStrLn (show err) >> notifyFail
    Right x   -> return x

  asmCode <- genASM tree'
  return asmCode

notifySuccess :: IO a
notifySuccess = do
  hPutStr stderr "OK\n"
  exitSuccess

notifyFail :: IO a
notifyFail = do
  hPutStr stderr "ERROR\n"
  exitFailure

notifyUsage :: IO ()
notifyUsage = do
  hPutStr stderr "ERROR\n"
  putStrLn $ unlines [
      "Error: Exactly 3 arguments were expected:",
      "  [-jvm/-llvm]  - architecture switch",
      "  input file",
      "  output file"
    ]
  exitFailure





