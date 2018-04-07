{-# LANGUAGE OverloadedStrings #-}

module AstSpec.ParseSpec where

import qualified Ast as AST
import qualified Ast.Parse as P
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Test.Hspec as Hspec
import Test.Hspec (describe, it, shouldBe)

errorPrefix :: String
errorPrefix = "Error in $: "

spec :: Hspec.Spec
spec =
    describe "AST parse" $ do
        describe "basics" $
            it "should have message on invalid json" $
            P.parsePrimitiveType "string\"" `shouldBe`
            Left (errorPrefix ++ "Failed reading: not a valid json value")
        describe "primitives" $ do
            describe "should successfully to parse" $ do
                it "a string" $ P.parsePrimitiveType "\"string\"" `shouldBe` Right AST.String
                it "a int" $ P.parsePrimitiveType "\"int\"" `shouldBe` Right AST.Int
                it "a float" $ P.parsePrimitiveType "\"float\"" `shouldBe` Right AST.Float
                it "a bool" $ P.parsePrimitiveType "\"bool\"" `shouldBe` Right AST.Bool
                it "a unit" $ P.parsePrimitiveType "[]" `shouldBe` Right AST.Unit
                it "an x-tuple" $
                    P.parsePrimitiveType "[\"string\", \"bool\", \"int\", \"float\"]" `shouldBe`
                    Right (AST.Tuple [AST.String, AST.Bool, AST.Int, AST.Float])
                it "an x-tuple with a sub record" $
                    P.parsePrimitiveType "[\"string\", {\"hello\": \"int\"}]" `shouldBe`
                    Right
                        (AST.Tuple [AST.String, AST.Record (HashMap.fromList [("hello", AST.Int)])])
                it "an x-tuple with a nested x-tuple" $
                    P.parsePrimitiveType "[[\"string\", \"bool\"], \"int\", \"float\"]" `shouldBe`
                    Right (AST.Tuple [AST.Tuple [AST.String, AST.Bool], AST.Int, AST.Float])
                it "a record" $
                    let json =
                            "{\n\
                    \   \"hello\": \"string\",\n\
                    \   \"world\": \"int\"\n\
                    \}"
                        expected =
                            (AST.Record $
                             HashMap.fromList [("hello", AST.String), ("world", AST.Int)])
                    in P.parsePrimitiveType json `shouldBe` Right expected
                it "a nested record" $
                    let json =
                            "{\n\
                        \   \"hello\": \"string\",\n\
                        \   \"world\": {\n\
                        \       \"foo\": \"string\",\n\
                        \       \"bar\": \"float\"\n\
                        \   }\n\
                        \}"
                        expected =
                            (AST.Record $
                             HashMap.fromList
                                 [ ("hello", AST.String)
                                 , ( "world"
                                   , AST.Record
                                         (HashMap.fromList [("foo", AST.String), ("bar", AST.Float)]))
                                 ])
                    in P.parsePrimitiveType json `shouldBe` Right expected
                it "an empty nested record" $
                    let json =
                            "{\n\
                        \   \"hello\": \"string\",\n\
                        \   \"world\": {\n\
                        \   }\n\
                        \}"
                        expected =
                            HashMap.fromList
                                [("hello", AST.String), ("world", AST.Record (HashMap.fromList []))]
                    in P.parsePrimitiveType json `shouldBe` Right (AST.Record expected)
            describe "should fail to parse" $ do
                it "an invalid type" $
                    P.parsePrimitiveType "\"dog\"" `shouldBe`
                    Left
                        (errorPrefix ++
                         "I was expecting one of \"int\", \"float\", \"bool\", \"string\" or an object, but got \"dog\" instead.\n")
                it "an invalid type, but provide a guess" $
                    P.parsePrimitiveType "\"Int\"" `shouldBe`
                    Left
                        (errorPrefix ++
                         "I got \"Int\" as a field type, which is invalid. Did you mean \"int\"?\n")
                it "an x-tuple if it has an invalid sub-type" $
                    P.parsePrimitiveType "[\"string\", \"ugh\"]" `shouldBe`
                    Left
                        (errorPrefix ++
                         "I was expecting one of \"int\", \"float\", \"bool\", \"string\" or an object, but got \"ugh\" instead.\n")
                it "an x-tuple with only 1 element" $
                    P.parsePrimitiveType "[\"string\"]" `shouldBe`
                    Left
                        (errorPrefix ++
                         "I got a tuple with one element, which is invalid. Please add more values, or don't put the type in a tuple!\n")
                it "a nested record with invalid sub-type" $
                    let json =
                            "{\n\
                    \   \"hello\": \"string\",\n\
                    \   \"world\": {\n\
                    \       \"foo\": \"bar\"\n\
                    \   }\n\
                    \}"
                        expected =
                            errorPrefix ++
                            "I was expecting one of \"int\", \"float\", \"bool\", \"string\" or an object, but got \"bar\" instead.\n"
                    in P.parsePrimitiveType json `shouldBe` Left expected
        describe "customTypes" $ do
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
                    in P.parseCustomType json `shouldBe` Left expected
                it "if the name field is the wrong type" $
                    let json =
                            "{\n\
                            \   \"name\": true,\n\
                            \   \"kind\": \"alias\",\n\
                            \   \"value\": \"string\"\n\
                            \}"
                        expected =
                            errorPrefix ++
                            "I was expecting the \"name\" field on the alias/union type to be of type \"string\", but it was Bool.\n"
                    in P.parseCustomType json `shouldBe` Left expected
                it "kind field is missing" $
                    let json =
                            "{\n\
                            \   \"name\": \"hello\",\n\
                            \   \"value\": \"string\"\n\
                            \}"
                        expected =
                            errorPrefix ++
                            "I was expecting the \"kind\" field to exist on the alias/union type, but I didn't find it!\n"
                    in P.parseCustomType json `shouldBe` Left expected
            describe "should fail to parse an alias type" $ do
                it "if value field is missing" $
                    let json =
                            "{\n\
                            \   \"name\": \"hello\",\n\
                            \   \"kind\": \"alias\"\n\
                            \}"
                        expected =
                            errorPrefix ++
                            "I was expecting the \"value\" field to exist on the alias type \"hello\", but I didn't find it!\n"
                    in P.parseCustomType json `shouldBe` Left expected
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
                    in P.parseCustomType json `shouldBe` Left expected
            describe "should fail to parse a union type" $ do
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
                    in P.parseCustomType json `shouldBe` Left expected
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
                    in P.parseCustomType json `shouldBe` Left expected
            describe "should successfully parse" $ do
                it "an alias type" $
                    let json =
                            "{\n\
                            \   \"name\": \"hello\",\n\
                            \   \"kind\": \"alias\",\n\
                            \   \"value\": \"string\"\n\
                            \}"
                        expected = AST.Alias "hello" AST.String
                    in P.parseCustomType json `shouldBe` Right expected
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
                            AST.Union
                                "Status"
                                (HashMap.fromList
                                     [ ("Bad", [AST.String, AST.Int])
                                     , ( "Good"
                                       , [AST.Record (HashMap.fromList [("hello", AST.String)])])
                                     ])
                    in P.parseCustomType json `shouldBe` Right expected

main :: IO ()
main = Hspec.hspec spec
