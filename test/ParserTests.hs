module ParserTests (
                    parserTests
                   ) where

import Test.Hspec
import Control.Exception

import LexerGrammar
import ParserGrammar

parserTests :: IO()
parserTests = hspec $
    describe "Parser tests" $ do
        it "basic test" $ do
            let tokens = alexScanTokens "program myKek;"
            tokens `shouldBe` [TokenPROGRAM, TokenIdentifier "myKek", TokenSEMI]
            pascalParse tokens `shouldBe` ()

