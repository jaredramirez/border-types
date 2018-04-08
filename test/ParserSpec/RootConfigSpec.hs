{-# LANGUAGE OverloadedStrings #-}

module ParserSpec.RootConfigSpec where

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified Parser.RootConfig as P
import qualified Test.Hspec as Hspec
import Test.Hspec (describe, it, shouldBe)
import qualified Types

main :: IO ()
main = Hspec.hspec spec

errorPrefix :: String
errorPrefix = "Error in $: "

spec :: Hspec.Spec
spec =
    describe "root config parser" $ do
        describe "should fail to parse" $ do
            it "if the \"langauges\" field is missing" $
                let json =
                        "{\n\
                        \   \"types\": [\n\
                        \       {\n\
                        \          \"name\": \"MyType\",\n\
                        \          \"kind\": \"alias\",\n\
                        \          \"value\": \"string\"\n\
                        \       }\n\
                        \   ]\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"langauges\" field to exists on the root config, but I didn't find it!\n"
                in P.parseConfig json `shouldBe` Left expected
            it "if the \"langauges\" field is the wrong type" $
                let json =
                        "{\n\
                        \   \"languages\": \"hello\",\n\
                        \   \"types\": [\n\
                        \       {\n\
                        \          \"name\": \"MyType\",\n\
                        \          \"kind\": \"alias\",\n\
                        \          \"value\": \"string\"\n\
                        \       }\n\
                        \   ]\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"langauges\" field to be an array, but it was of type String.\n"
                in P.parseConfig json `shouldBe` Left expected
            it "if the \"langauges\" field has a child of the wrong type" $
                let json =
                        "{\n\
                        \   \"languages\": [\n\
                        \       true\n\
                        \   ],\n\
                        \   \"types\": [\n\
                        \       {\n\
                        \          \"name\": \"MyType\",\n\
                        \          \"kind\": \"alias\",\n\
                        \          \"value\": \"string\"\n\
                        \       }\n\
                        \   ]\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"langauges\" field to be an array of language config objecs, but one of the children was of type Bool.\n"
                in P.parseConfig json `shouldBe` Left expected
            it "if the \"langauges\" field has an invalid child" $
                let json =
                        "{\n\
                        \   \"languages\": [\n\
                        \       { \"name\": \"elm\" }\n\
                        \   ],\n\
                        \   \"types\": [\n\
                        \       {\n\
                        \          \"name\": \"MyType\",\n\
                        \          \"kind\": \"alias\",\n\
                        \          \"value\": \"string\"\n\
                        \       }\n\
                        \   ]\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "There's an issue with one the language config objects. I was expecting the \"outputPath\" field to exist on the language config for \"elm\", but I didn't find it!\n"
                in P.parseConfig json `shouldBe` Left expected
            it "if the \"types\" field is missing" $
                let json =
                        "{\n\
                        \   \"languages\": [\n\
                        \       {\n\
                        \           \"name\": \"reason\",\n\
                        \           \"outputPath\": \"./hello/world\"\n\
                        \       }\n\
                        \   ]\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"types\" field to exists on the root config, but I didn't find it!\n"
                in P.parseConfig json `shouldBe` Left expected
            it "if the \"types\" field is an invalid type" $
                let json =
                        "{\n\
                        \   \"languages\": [\n\
                        \       {\n\
                        \           \"name\": \"reason\",\n\
                        \           \"outputPath\": \"./hello/world\"\n\
                        \       }\n\
                        \   ],\n\
                        \   \"types\": \"string\"\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"types\" field to be an array, but it was of type String.\n"
                in P.parseConfig json `shouldBe` Left expected
            it "if the \"types\" field is an invalid type" $
                let json =
                        "{\n\
                        \   \"languages\": [\n\
                        \       {\n\
                        \           \"name\": \"reason\",\n\
                        \           \"outputPath\": \"./hello/world\"\n\
                        \       }\n\
                        \   ],\n\
                        \   \"types\": [\n\
                        \       []\n\
                        \   ]\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"types\" field to be an array of type definitons, but one of the children was of type Array.\n"
                in P.parseConfig json `shouldBe` Left expected
            it "if the \"types\" field has an invalid child" $
                let json =
                        "{\n\
                        \   \"languages\": [\n\
                        \       {\n\
                        \           \"name\": \"reason\",\n\
                        \           \"outputPath\": \"./hello/world\"\n\
                        \       }\n\
                        \   ],\n\
                        \   \"types\": [\n\
                        \       {\n\
                        \          \"name\": \"MyType\",\n\
                        \          \"kind\": \"alias\"\n\
                        \       }\n\
                        \   ]\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "There's an issue with one the type definitions in the \"types\" field. I was expecting the \"value\" field to exist on the alias type \"MyType\", but I didn't find it!\n"
                in P.parseConfig json `shouldBe` Left expected
        describe "should successfully parse" $
            it "a valid config object" $
            let json =
                    "{\n\
                        \   \"languages\": [\n\
                        \       {\n\
                        \           \"name\": \"elm\",\n\
                        \           \"outputPath\": \"./src/elm/data\"\n\
                        \       },\n\
                        \       {\n\
                        \           \"name\": \"reason\",\n\
                        \           \"outputPath\": \"./src/reason/data\"\n\
                        \       }\n\
                        \   ],\n\
                        \   \"types\": [\n\
                        \       {\n\
                        \           \"name\": \"MyType\",\n\
                        \           \"kind\": \"alias\",\n\
                        \           \"value\": \"string\"\n\
                        \       },\n\
                        \       {\n\
                        \          \"name\": \"Status\",\n\
                        \          \"kind\": \"union\",\n\
                        \          \"constructors\": {\n\
                        \              \"Good\": [\n\
                        \                { \"my\": \"string\" }\n\
                        \              ],\n\
                        \              \"Bad\": [\n\
                        \                \"string\"\n\
                        \              ]\n\
                        \          }\n\
                        \       }\n\
                        \   ]\n\
                        \}"
                expected =
                    Types.Config
                        [Types.ElmConfig "./src/elm/data", Types.ReasonConfig "./src/reason/data"]
                        [ Types.Alias "MyType" Types.String
                        , Types.Union
                              "Status"
                              (HashMap.fromList
                                   [ ("Bad", [Types.String])
                                   , ( "Good"
                                     , [Types.Record (HashMap.fromList [("my", Types.String)])])
                                   ])
                        ]
            in P.parseConfig json `shouldBe` Right expected
