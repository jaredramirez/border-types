{-# LANGUAGE OverloadedStrings #-}

module GenerateSpec.ReasonSpec where

import qualified Data.HashMap.Strict as HashMap
import qualified Generate.Reason     as GenReason
import           Test.Hspec          (describe, it, shouldBe)
import qualified Test.Hspec          as Hspec
import qualified Types

main :: IO ()
main = Hspec.hspec spec

spec :: Hspec.Spec
spec =
  describe "reason type generation" $ do
    describe "primitive types" $ do
      it "should generate string" $
        GenReason.toPrimitiveType Types.String `shouldBe`
        Types.TypeString "string"
      it "should generate int" $
        GenReason.toPrimitiveType Types.Int `shouldBe` Types.TypeString "int"
      it "should generate float" $
        GenReason.toPrimitiveType Types.Float `shouldBe`
        Types.TypeString "float"
      it "should generate bool" $
        GenReason.toPrimitiveType Types.Bool `shouldBe` Types.TypeString "bool"
      it "should generate unit" $
        GenReason.toPrimitiveType Types.Unit `shouldBe` Types.TypeString "unit"
      it "should generate tuple" $
        GenReason.toPrimitiveType (Types.Tuple [Types.String, Types.Int]) `shouldBe`
        Types.TypeString "(string, int)"
      it "should generate record" $
        GenReason.toPrimitiveType
          (Types.Record $
           HashMap.fromList [("hello", Types.String), ("world", Types.Int)]) `shouldBe`
        Types.TypeString
          "{.\n\
          \  \"world\": int,\n\
          \  \"hello\": string\n\
          \}"
    describe "custom types" $ do
      it "should generate alias type" $
        GenReason.toCustomType
          (Types.Alias
             "Hello"
             (Types.Record $
              HashMap.fromList [("hello", Types.String), ("world", Types.Int)])) `shouldBe`
        Types.TypeString
          "type hello = {.\n\
          \  \"world\": int,\n\
          \  \"hello\": string\n\
          \}"
      it "should generate union type" $
        GenReason.toCustomType
          (Types.Union
             "Status"
             (HashMap.fromList
                [("Good", [Types.String]), ("Okay", []), ("Bad", [Types.Int])])) `shouldBe`
        Types.TypeString
          "type status =\n\
          \  | Okay\n\
          \  | Bad(int)\n\
          \  | Good(string);"
