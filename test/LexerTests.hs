module LexerTests (
                    lexerTests
                  ) where

import Test.Hspec
import Control.Exception

import LexerGrammar

lexerTests :: IO()
lexerTests = hspec $
    describe "Lexer tests" $ do
        it "parse empty" $ do scanTokens "" `shouldBe` []
        it "parse and" $ do scanTokens "and" `shouldBe` [TokenAND]
        it "parse aNd" $ do scanTokens "aNd" `shouldBe` [TokenAND]
        it "parse rel operators" $ do 
            scanTokens "< > <= >= <> =  " 
                `shouldBe` [TokenLT, TokenGT, TokenLE, TokenGE, TokenNEQ, TokenEQ]
        it "parse brackets" $ do
            scanTokens "( )" `shouldBe` [TokenLPAREN, TokenRPAREN]
            scanTokens "{ }" `shouldBe` [] -- comment section
            scanTokens "[ ]" `shouldBe` [TokenLBRACKET, TokenRBRACKET]
            scanTokens ". .." `shouldBe` [TokenDOT, TokenDOTDOT]
        it "parse var decl" $ do
            scanTokens "var x: integer;" 
                `shouldBe` [TokenVAR, TokenIdentifier "x", TokenCOLON, TokenINTEGER, TokenSEMI]
        it "parse integer" $ do
            scanTokens "42" `shouldBe` [TokenInteger 42]
        it "parse signed" $ do
            scanTokens "-42" `shouldBe` [TokenMINUS, TokenInteger 42]
        it "parse real" $ do
            scanTokens "42.5" `shouldBe` [TokenReal 42.5]
        it "parse numbers" $ do
            scanTokens "42 -42 4.0" 
                `shouldBe` [TokenInteger 42, TokenMINUS, TokenInteger 42, TokenReal 4.0]


