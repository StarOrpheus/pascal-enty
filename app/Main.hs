module Main where

import System.Environment
import System.IO

import Data.List
import Text.Pretty.Simple (pPrint)

import Formatter
import Grammar
import LexerGrammar
import ParserGrammar

import Interpreter

readContent :: FilePath -> IO String
readContent fname = do
    code <- readFile fname
    return $ seq (length code) code

interpret :: FilePath -> IO ()
interpret fileName = do
    code <- readContent fileName
    let ast' = parseExpr code
    case ast' of
        Left error -> putStrLn error
        Right ast  -> runProgram ast

pprint :: FilePath -> IO ()
pprint fileName = do
    code <- readContent fileName
    let ast = parseExpr code
    case ast of
        Left error -> putStrLn error
        Right ast -> do
            let lines = pascalFormat ast
            foldl' (\acc str -> acc >> putStrLn str) (return ()) lines

dumpAst :: FilePath -> IO ()
dumpAst fname = do
    code <- readContent fname
    let tokens = scanTokens code
    pPrint tokens
    let ast = parseTokens tokens
    pPrint ast

main :: IO ()
main = do
    [runFormat, filename] <- getArgs
    case runFormat of
        "run"    -> interpret filename
        "pprint" -> pprint filename
        "dump"   -> dumpAst filename
        _        -> error "Unexpected run format!\nUsage enty (run|pprint) filename.pas"
