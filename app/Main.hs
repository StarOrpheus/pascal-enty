module Main where

import System.Environment
import System.IO

import Text.Pretty.Simple (pPrint)

import Grammar
import LexerGrammar
import ParserGrammar

readContent :: FilePath -> IO String
readContent fname = do
    fileHandler <- openFile fname ReadMode
    code <- hGetContents fileHandler
    return code

interpret :: FilePath -> IO ()
interpret kek = do
    return ()

pprint :: FilePath -> IO ()
pprint fileName = do
    return ()

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
        "run" -> interpret filename
        "pprint" -> pprint filename
        "dump" -> dumpAst filename
        otherwise -> error "Unexpected run format!\nUsage enty (run|pprint) filename.pas"
