{-# LANGUAGE OverloadedStrings #-}

module ParserSpec.LanguageConfigSpec where

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified Parser.LanguageConfig as P
import qualified Test.Hspec as Hspec
import Test.Hspec (describe, it, shouldBe)
import qualified Types

main :: IO ()
main = Hspec.hspec spec

errorPrefix :: String
errorPrefix = "Error in $: "

spec :: Hspec.Spec
spec =
    describe "language config parser" $ do
        describe "should fail to parse" $ do
            it "if the \"name\" field is missing" $
                let json =
                        "{\n\
                        \   \"outputPath\": \"./hello/world\"\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"name\" field to exists on the language config, but I didn't find it!\n"
                in P.parseString json `shouldBe` Left expected
            it "if the \"name\" field is an invalid type" $
                let json =
                        "{\n\
                    \   \"name\": true,\n\
                    \   \"outputPath\": \"./hello/world\"\n\
                    \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"name\" field on the language config to be a \"string\" , but it was of type Bool.\n"
                in P.parseString json `shouldBe` Left expected
            it "if the \"name\" field is invalid" $
                let json =
                        "{\n\
                    \   \"name\": \"pink\",\n\
                    \   \"outputPath\": \"./hello/world\"\n\
                    \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"name\" field on the language config to be \"elm\" or \"reason\", but it was \"pink\".\n"
                in P.parseString json `shouldBe` Left expected
            it "if the \"outputPath\" field is missing" $
                let json =
                        "{\n\
                        \   \"name\": \"reason\"\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"outputPath\" field to exist on the language config for \"reason\", but I didn't find it!\n"
                in P.parseString json `shouldBe` Left expected
            it "if the \"outputPath\" field is an invalid type" $
                let json =
                        "{\n\
                    \   \"name\": \"reason\",\n\
                    \   \"outputPath\": []\n\
                    \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"outputPath\" field on the language config for \"reason\" to be a \"string\" , but it was of type Array.\n"
                in P.parseString json `shouldBe` Left expected
        describe "should successfully parse" $ do
            it "the elm language config" $
                let json =
                        "{\n\
                    \   \"name\": \"elm\",\n\
                    \   \"outputPath\": \"./hello/world\"\n\
                    \}"
                    expected = Types.ElmConfig "./hello/world"
                in P.parseString json `shouldBe` Right expected
            it "the reason language config" $
                let json =
                        "{\n\
                    \   \"name\": \"reason\",\n\
                    \   \"outputPath\": \"./hello/world\"\n\
                    \}"
                    expected = Types.ReasonConfig "./hello/world"
                in P.parseString json `shouldBe` Right expected
