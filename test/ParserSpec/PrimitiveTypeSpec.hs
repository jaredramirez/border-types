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
        describe "should successfully to parse" $ do
            it "a string" $ P.parsePrimitiveType "\"string\"" `shouldBe` Right Types.String
            it "a int" $ P.parsePrimitiveType "\"int\"" `shouldBe` Right Types.Int
            it "a float" $ P.parsePrimitiveType "\"float\"" `shouldBe` Right Types.Float
            it "a bool" $ P.parsePrimitiveType "\"bool\"" `shouldBe` Right Types.Bool
            it "a list" $
                P.parsePrimitiveType "[\"string\"]" `shouldBe` Right (Types.List Types.String)
            it "a unit" $ P.parsePrimitiveType "[]" `shouldBe` Right Types.Unit
            it "an x-tuple" $
                P.parsePrimitiveType "[\"string\", \"bool\", \"int\", \"float\"]" `shouldBe`
                Right (Types.Tuple [Types.String, Types.Bool, Types.Int, Types.Float])
            it "an x-tuple with a sub record" $
                P.parsePrimitiveType "[\"string\", {\"hello\": \"int\"}]" `shouldBe`
                Right
                    (Types.Tuple
                         [Types.String, Types.Record (HashMap.fromList [("hello", Types.Int)])])
            it "an x-tuple with a nested x-tuple" $
                P.parsePrimitiveType "[[\"string\", \"bool\"], \"int\", \"float\"]" `shouldBe`
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
                        (Types.Record $
                         HashMap.fromList
                             [ ("hello", Types.String)
                             , ( "world"
                               , Types.Record
                                     (HashMap.fromList [("foo", Types.String), ("bar", Types.Float)]))
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
                            [("hello", Types.String), ("world", Types.Record (HashMap.fromList []))]
                in P.parsePrimitiveType json `shouldBe` Right (Types.Record expected)
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