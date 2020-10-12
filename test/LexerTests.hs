{-# LANGUAGE TemplateHaskell #-}

module LexerTests (
                    lexerTests
                  ) where

import Test.Hspec
import Control.Exception

import LexerGrammar

lexerTests :: IO()
lexerTests = hspec $
    describe "Lexer tests" $ do
        it "parse empty" $ do alexScanTokens "" `shouldBe` []
        it "parse and" $ do alexScanTokens "and" `shouldBe` [TokenAND]
        it "parse aNd" $ do alexScanTokens "aNd" `shouldBe` [TokenAND]
        it "parse rel operators" $ do 
            alexScanTokens "< > <= >= <> =  " 
                `shouldBe` [TokenLT, TokenGT, TokenLE, TokenGE, TokenNEQ, TokenEQ]
        it "parse brackets" $ do
            alexScanTokens "( )" `shouldBe` [TokenLPAREN, TokenRPAREN]
            alexScanTokens "{ }" `shouldBe` [] -- comment section
            alexScanTokens "[ ]" `shouldBe` [TokenLBRACKET, TokenRBRACKET]
            alexScanTokens ". .." `shouldBe` [TokenDOT, TokenDOTDOT]
        it "parse var decl" $ do
            alexScanTokens "var x: integer;" 
                `shouldBe` [TokenVAR, TokenIdentifier "x", TokenCOLON, TokenINTEGER, TokenSEMI]
        it "parse integer" $ do
            alexScanTokens "42" `shouldBe` [TokenInteger 42]
        it "parse signed" $ do
            alexScanTokens "-42" `shouldBe` [TokenMINUS, TokenInteger 42]
        it "parse real" $ do
            alexScanTokens "42.5" `shouldBe` [TokenReal 42.5]
        it "parse numbers" $ do
            alexScanTokens "42 -42 4.0" 
                `shouldBe` [TokenInteger 42, TokenMINUS, TokenInteger 42, TokenReal 4.0]


