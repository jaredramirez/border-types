{-# LANGUAGE OverloadedStrings #-}

module GenerateSpec.ElmSpec where

import qualified Data.HashMap.Strict as HashMap
import qualified Generate.Elm        as GenElm
import qualified Generate.Reason     as GenReason
import           Test.Hspec          (describe, it, shouldBe)
import qualified Test.Hspec          as Hspec
import qualified Types

main :: IO ()
main = Hspec.hspec spec

spec :: Hspec.Spec
spec =
  describe "elm type generation" $ do
    describe "module" $
      it "should generate string" $
      GenElm.toModule "Hello" `shouldBe`
      Types.TypeString "module Hello exposing (..)\n"
    describe "primitive types" $ do
      it "should generate string" $
        GenElm.toPrimitiveType Types.String `shouldBe` Types.TypeString "String"
      it "should generate int" $
        GenElm.toPrimitiveType Types.Int `shouldBe` Types.TypeString "Int"
      it "should generate Float" $
        GenElm.toPrimitiveType Types.Float `shouldBe` Types.TypeString "Float"
      it "should generate bool" $
        GenElm.toPrimitiveType Types.Bool `shouldBe` Types.TypeString "Bool"
      it "should generate unit" $
        GenElm.toPrimitiveType Types.Unit `shouldBe` Types.TypeString "()"
      it "should generate tuple" $
        GenElm.toPrimitiveType (Types.Tuple [Types.String, Types.Int]) `shouldBe`
        Types.TypeString "(String, Int)"
      it "should generate record" $
        GenElm.toPrimitiveType
          (Types.Record $
           HashMap.fromList [("hello", Types.String), ("world", Types.Int)]) `shouldBe`
        Types.TypeString
          "{ world : Int\n\
          \, hello : String\n\
          \}"
    describe "custom types" $ do
      it "should generate alias record type" $
        GenElm.toCustomType
          (Types.Alias
             "Hello"
             (Types.Record $
              HashMap.fromList [("hello", Types.String), ("world", Types.Int)])) `shouldBe`
        Types.TypeString
          "type alias Hello =\n\
          \    { world : Int\n\
          \    , hello : String\n\
          \    }"
      it "should generate string type" $
        GenElm.toCustomType (Types.Alias "Hello" Types.String) `shouldBe`
        Types.TypeString
          "type alias Hello =\n\
          \    String"
      it "should generate union type" $
        GenElm.toCustomType
          (Types.Union
             "Status"
             (HashMap.fromList
                [("Good", [Types.String]), ("Okay", []), ("Bad", [Types.Int])])) `shouldBe`
        Types.TypeString
          "type Status\n\
          \    = Okay\n\
          \    | Bad Int\n\
          \    | Good String\n"
      it "should generate union typewith record" $
        GenElm.toCustomType
          (Types.Union
             "Status"
             (HashMap.fromList
                [ ("Good", [Types.String])
                , ("Okay", [])
                , ( "Bad"
                  , [ Types.Record $
                      HashMap.fromList
                        [("hello", Types.String), ("world", Types.Int)]
                    ])
                ])) `shouldBe`
        Types.TypeString
          "type Status\n\
          \    = Okay\n\
          \    | Bad\n\
          \        { world : Int\n\
          \        , hello : String\n\
          \        }\n\
          \    | Good String\n"
