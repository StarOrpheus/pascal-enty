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
        it "parse and" $ do alexScanTokens "and" `shouldBe` [TokenAnd]
        it "parse aNd" $ do alexScanTokens "aNd" `shouldBe` [TokenAnd]

