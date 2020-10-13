module Main (
               main
            ) where 

import LexerTests
import ParserTests

main :: IO ()
main = do
    lexerTests
    parserTests
    