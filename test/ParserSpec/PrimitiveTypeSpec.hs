{-# LANGUAGE OverloadedStrings #-}

module ParserSpec.PrimitiveTypeSpec where

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified Parser.PrimitiveType as P
import qualified Test.Hspec as Hspec
import Test.Hspec (describe, it, shouldBe)
import qualified Types

main :: IO ()
main = Hspec.hspec spec

errorPrefix :: String
errorPrefix = "Error in $: "

spec :: Hspec.Spec
spec =
    describe "primitives parser" $ do
        describe "should fail to parse" $ do
            it "an invalid type" $
                P.parseString "\"dog\"" `shouldBe`
                Left
                    (errorPrefix ++
                     "I was expecting one of \"int\", \"float\", \"bool\", \"string\" or an object, but got \"dog\" instead.\n")
            it "an x-tuple if it has an invalid sub-type" $
                P.parseString "[\"string\", \"ugh\"]" `shouldBe`
                Left
                    (errorPrefix ++
                     "I was expecting one of \"int\", \"float\", \"bool\", \"string\" or an object, but got \"ugh\" instead.\n")
            it "a record with an invalid key naem" $
                let json =
                        "{\n\
                        \   \"Hello\": \"string\"\n\
                        \}"
                    expected =
                        errorPrefix ++
                        "I was each key on a record to start with a lowercase letter, but I got the following keys: \"Hello\".\n"
                in P.parseString json `shouldBe` Left expected
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
                in P.parseString json `shouldBe` Left expected
        describe "should successfully to parse" $ do
            it "a string" $ P.parseString "\"string\"" `shouldBe` Right Types.String
            it "a int" $ P.parseString "\"int\"" `shouldBe` Right Types.Int
            it "a float" $ P.parseString "\"float\"" `shouldBe` Right Types.Float
            it "a bool" $ P.parseString "\"bool\"" `shouldBe` Right Types.Bool
            it "a list" $ P.parseString "[\"string\"]" `shouldBe` Right (Types.List Types.String)
            it "a unit" $ P.parseString "[]" `shouldBe` Right Types.Unit
            it "an x-tuple" $
                P.parseString "[\"string\", \"bool\", \"int\", \"float\"]" `shouldBe`
                Right (Types.Tuple [Types.String, Types.Bool, Types.Int, Types.Float])
            it "an x-tuple with a sub record" $
                P.parseString "[\"string\", {\"hello\": \"int\"}]" `shouldBe`
                Right
                    (Types.Tuple
                         [Types.String, Types.Record (HashMap.fromList [("hello", Types.Int)])])
            it "an x-tuple with a nested x-tuple" $
                P.parseString "[[\"string\", \"bool\"], \"int\", \"float\"]" `shouldBe`
                Right (Types.Tuple [Types.Tuple [Types.String, Types.Bool], Types.Int, Types.Float])
            it "a record" $
                let json =
                        "{\n\
                \   \"hello\": \"string\",\n\
                \   \"world\": \"int\"\n\
                \}"
                    expected =
                        (Types.Record $
                         HashMap.fromList [("hello", Types.String), ("world", Types.Int)])
                in P.parseString json `shouldBe` Right expected
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
                        (Types.Record $
                         HashMap.fromList
                             [ ("hello", Types.String)
                             , ( "world"
                               , Types.Record
                                     (HashMap.fromList [("foo", Types.String), ("bar", Types.Float)]))
                             ])
                in P.parseString json `shouldBe` Right expected
            it "an empty nested record" $
                let json =
                        "{\n\
                    \   \"hello\": \"string\",\n\
                    \   \"world\": {\n\
                    \   }\n\
                    \}"
                    expected =
                        HashMap.fromList
                            [("hello", Types.String), ("world", Types.Record (HashMap.fromList []))]
                in P.parseString json `shouldBe` Right (Types.Record expected)
