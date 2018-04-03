{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import qualified Data.HashMap.Strict as HashMap
import qualified Parser as P
import qualified Test.Hspec as Hspec
import Test.Hspec (describe, it, shouldBe)

spec :: Hspec.Spec
spec =
    describe "Parser" $
    describe "Primitives" $ do
        it "should have  message on invalid json" $
            P.decodePrimitiveType "string\"" `shouldBe`
            Left "Error in $: Failed reading: not a valid json value"
        it "should successfully decode string" $
            P.decodePrimitiveType "\"string\"" `shouldBe` Right P.String
        it "should successfully decode int" $ P.decodePrimitiveType "\"int\"" `shouldBe` Right P.Int
        it "should successfully decode float" $
            P.decodePrimitiveType "\"float\"" `shouldBe` Right P.Float
        it "should successfully decode bool" $
            P.decodePrimitiveType "\"bool\"" `shouldBe` Right P.Bool
        it "should fail to decode invalid types" $
            P.decodePrimitiveType "\"dog\"" `shouldBe`
            Left
                "Error in $: Uh oh, I was expecting one of \"int\", \"float\", \"bool\", \"string\" or an object, but got \"dog\" instead.\n"
        it "should fail and provide guess when decode similar invalid types" $
            P.decodePrimitiveType "\"Int\"" `shouldBe`
            Left
                "Error in $: Uh oh, I got \"Int\" as a field type, which is invalid. Did you mean \"int\"?\n"
        it "should successfully decode record" $
            let json =
                    "{\n\
                    \   \"hello\": \"string\",\n\
                    \   \"world\": \"int\"\n\
                    \}"
                expected = HashMap.fromList [("hello", P.String), ("world", P.Int)]
            in P.decodePrimitiveType json `shouldBe` Right (P.Record expected)
        it "should successfully decode nested record" $
            let json =
                    "{\n\
                    \   \"hello\": \"string\",\n\
                    \   \"world\": {\n\
                    \       \"foo\": \"string\",\n\
                    \       \"bar\": \"float\"\n\
                    \   }\n\
                    \}"
                expected =
                    HashMap.fromList
                        [ ("hello", P.String)
                        , ( "world"
                          , P.Record (HashMap.fromList [("foo", P.String), ("bar", P.Float)]))
                        ]
            in P.decodePrimitiveType json `shouldBe` Right (P.Record expected)
        it "should successfully decode empty nested record" $
            let json =
                    "{\n\
                    \   \"hello\": \"string\",\n\
                    \   \"world\": {\n\
                    \   }\n\
                    \}"
                expected =
                    HashMap.fromList
                        [("hello", P.String), ("world", P.Record (HashMap.fromList []))]
            in P.decodePrimitiveType json `shouldBe` Right (P.Record expected)
        it "should fail decode nested record with invalid sub-type" $
            let json =
                    "{\n\
                    \   \"hello\": \"string\",\n\
                    \   \"world\": {\n\
                    \       \"foo\": \"bar\"\n\
                    \   }\n\
                    \}"
                expected =
                    "Error in $: Uh oh, I was expecting one of \"int\", \"float\", \"bool\", \"string\" or an object, but got \"bar\" instead.\n"
            in P.decodePrimitiveType json `shouldBe` Left expected
