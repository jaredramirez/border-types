{-# LANGUAGE OverloadedStrings #-}

module ParserSpec.CustomTypeSpec where

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified Parser.CustomType as P
import qualified Test.Hspec as Hspec
import Test.Hspec (describe, it, shouldBe)
import qualified Types

main :: IO ()
main = Hspec.hspec spec

errorPrefix :: String
errorPrefix = "Error in $: "

spec :: Hspec.Spec
spec =
    describe "custom types parser" $ do
        describe "should fail to parse" $ do
            it "if the name field is missing" $
                let json =
                        "{\n\
                        \   \"kind\": \"alias\",\n\
                        \   \"value\": \"string\"\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"name\" field to exist on the alias/union type, but I didn't find it!\n"
                in P.parseString json `shouldBe` Left expected
            it "if the name field is an invalid type" $
                let json =
                        "{\n\
                        \   \"name\": true,\n\
                        \   \"kind\": \"alias\",\n\
                        \   \"value\": \"string\"\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"name\" field on the alias/union type to be of type \"string\", but it was Bool.\n"
                in P.parseString json `shouldBe` Left expected
            it "kind field is missing" $
                let json =
                        "{\n\
                        \   \"name\": \"hello\",\n\
                        \   \"value\": \"string\"\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"kind\" field to exist on the alias/union type, but I didn't find it!\n"
                in P.parseString json `shouldBe` Left expected
            describe "an alias type" $ do
                it "if value field is missing" $
                    let json =
                            "{\n\
                            \   \"name\": \"hello\",\n\
                            \   \"kind\": \"alias\"\n\
                            \}"
                        expected =
                            errorPrefix ++
                            "I was expecting the \"value\" field to exist on the alias type \"hello\", but I didn't find it!\n"
                    in P.parseString json `shouldBe` Left expected
                it "if value field is an invalid type" $
                    let json =
                            "{\n\
                            \   \"name\": \"bad\",\n\
                            \   \"kind\": \"alias\",\n\
                            \   \"value\": \"abcdefg\"\n\
                            \}"
                        expected =
                            errorPrefix ++
                            "There's an issue in the alias type \"bad\". I was expecting one of \"int\", \"float\", \"bool\", \"string\" or an object, but got \"abcdefg\" instead.\n"
                    in P.parseString json `shouldBe` Left expected
            describe "a union type" $ do
                it "if the constructors field is not an object" $
                    let json =
                            "{\n\
                            \   \"name\": \"bad\",\n\
                            \   \"kind\": \"union\",\n\
                            \   \"constructors\": \"abcdefg\"\n\
                            \}"
                        expected =
                            errorPrefix ++
                            "On the \"bad\" union type, I was expecting the \"constructors\" field to be a record, but it was of type String.\n"
                    in P.parseString json `shouldBe` Left expected
                it "if not all constructors have valid primitive types" $
                    let json =
                            "{\n\
                            \   \"name\": \"Bad\",\n\
                            \   \"kind\": \"union\",\n\
                            \   \"constructors\": {\n\
                            \       \"Bad\": [\n\
                            \           \"string\",\n\
                            \           \"ugh\"\n\
                            \      ]\n\
                            \   }\n\
                            \}"
                        expected =
                            errorPrefix ++
                            "There's an issue with one of the constructor values on the union type \"Bad\". I was expecting one of \"int\", \"float\", \"bool\", \"string\" or an object, but got \"ugh\" instead.\n"
                    in P.parseString json `shouldBe` Left expected
        describe "should successfully parse" $ do
            it "an alias type" $
                let json =
                        "{\n\
                        \   \"name\": \"hello\",\n\
                        \   \"kind\": \"alias\",\n\
                        \   \"value\": \"string\"\n\
                        \}"
                    expected = Types.Alias "hello" Types.String
                in P.parseString json `shouldBe` Right expected
            it "a union type" $
                let json =
                        "{\n\
                        \   \"name\": \"Status\",\n\
                        \   \"kind\": \"union\",\n\
                        \   \"constructors\": {\n\
                        \       \"Bad\": [\n\
                        \           \"string\",\n\
                        \           \"int\"\n\
                        \      ],\n\
                        \       \"Good\": [{\n\
                        \           \"hello\": \"string\"\n\
                        \      }]\n\
                        \   }\n\
                        \}"
                    expected =
                        Types.Union
                            "Status"
                            (HashMap.fromList
                                 [ ("Bad", [Types.String, Types.Int])
                                 , ( "Good"
                                   , [Types.Record (HashMap.fromList [("hello", Types.String)])])
                                 ])
                in P.parseString json `shouldBe` Right expected
