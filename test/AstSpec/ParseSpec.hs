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
    describe "Parser" $ do
        describe "Basic" $
            it "should have message on invalid json" $
            P.parsePrimitiveType "string\"" `shouldBe`
            Left (errorPrefix ++ "Failed reading: not a valid json value")
        describe "Primitives" $ do
            it "should fail to parse invalid types" $
                P.parsePrimitiveType "\"dog\"" `shouldBe`
                Left
                    (errorPrefix ++
                     "I was expecting one of \"int\", \"float\", \"bool\", \"string\" or an object, but got \"dog\" instead.\n")
            it "should fail and provide guess when parse similar invalid types" $
                P.parsePrimitiveType "\"Int\"" `shouldBe`
                Left
                    (errorPrefix ++
                     "I got \"Int\" as a field type, which is invalid. Did you mean \"int\"?\n")
            it "should successfully parse string" $
                P.parsePrimitiveType "\"string\"" `shouldBe` Right AST.String
            it "should successfully parse int" $
                P.parsePrimitiveType "\"int\"" `shouldBe` Right AST.Int
            it "should successfully parse float" $
                P.parsePrimitiveType "\"float\"" `shouldBe` Right AST.Float
            it "should successfully parse bool" $
                P.parsePrimitiveType "\"bool\"" `shouldBe` Right AST.Bool
            it "should fail parse nested record with invalid sub-type" $
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
            it "should successfully parse record" $
                let json =
                        "{\n\
                    \   \"hello\": \"string\",\n\
                    \   \"world\": \"int\"\n\
                    \}"
                    expected =
                        (AST.Record $ HashMap.fromList [("hello", AST.String), ("world", AST.Int)])
                in P.parsePrimitiveType json `shouldBe` Right expected
            it "should successfully parse nested record" $
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
            it "should successfully parse empty nested record" $
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
        describe "Custom Type" $ do
            it "should fail to parse if name is missing" $
                let json =
                        "{\n\
                        \   \"kind\": \"alias\",\n\
                        \   \"value\": \"string\"\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"name\" field to exist on the alias/union type, but I didn't find it!\n"
                in P.parseCustomType json `shouldBe` Left expected
            it "should fail to parse if name wrong type" $
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
            it "should fail to parse if kind is missing" $
                let json =
                        "{\n\
                        \   \"name\": \"hello\",\n\
                        \   \"value\": \"string\"\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"kind\" field to exist on the alias/union type, but I didn't find it!\n"
                in P.parseCustomType json `shouldBe` Left expected
            it "should fail to parse alias type if value is missing" $
                let json =
                        "{\n\
                        \   \"name\": \"hello\",\n\
                        \   \"kind\": \"alias\"\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was expecting the \"value\" field to exist on the alias type \"hello\", but I didn't find it!\n"
                in P.parseCustomType json `shouldBe` Left expected
            it "should fail to parse alias type if value is an invalid type" $
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
            it "should parse alias type" $
                let json =
                        "{\n\
                        \   \"name\": \"hello\",\n\
                        \   \"kind\": \"alias\",\n\
                        \   \"value\": \"string\"\n\
                        \}"
                    expected = AST.Alias "hello" AST.String
                in P.parseCustomType json `shouldBe` Right expected
            it "should fail to parse union type if constructors is not an object" $
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
            it "should fail to parse union type if not all constructors have valid values" $
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
            it "should successfully to parse union type" $
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
                                 , ("Good", [AST.Record (HashMap.fromList [("hello", AST.String)])])
                                 ])
                in P.parseCustomType json `shouldBe` Right expected

main :: IO ()
main = Hspec.hspec spec
