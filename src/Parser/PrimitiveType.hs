{-# LANGUAGE OverloadedStrings #-}

module Parser.PrimitiveType
    ( parseString
    , parseJsonValue
    , displayParseError
    , ParseError
    ) where

import qualified Data.Aeson as JsonBase
import qualified Data.Aeson.BetterErrors as Json
import qualified Data.Aeson.Types as JsonTypes
import qualified Data.ByteString.Lazy as BS
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Parser.Misc as PMisc
import qualified Types

parseString :: BS.ByteString -> Either String Types.PrimitiveType
parseString = JsonBase.eitherDecode

instance JsonTypes.FromJSON Types.PrimitiveType where
    parseJSON = Json.toAesonParser displayParseError (Json.withValue parseJsonValue)

data ParseError
    = UnknownType Text.Text
    | InvalidRecordKeys [Text.Text]
    | InvalidType JsonTypes.Value
    deriving (Show)

intJsonValue :: Text.Text
intJsonValue = "int"

floatJsonValue :: Text.Text
floatJsonValue = "float"

boolJsonValue :: Text.Text
boolJsonValue = "bool"

stringJsonValue :: Text.Text
stringJsonValue = "string"

parseJsonValue :: JsonTypes.Value -> Either ParseError Types.PrimitiveType
parseJsonValue value =
    case value of
        (JsonTypes.String s)
            | s == intJsonValue -> Right Types.Int
            | s == floatJsonValue -> Right Types.Float
            | s == boolJsonValue -> Right Types.Bool
            | s == stringJsonValue -> Right Types.String
            | otherwise -> Left $ UnknownType s
        (JsonTypes.Array v) ->
            let list = Vector.toList v
            in case list of
                   [] -> Right Types.Unit
                   [listValue] -> parseJsonValue listValue >>= (Right . Types.List)
                   tupleValues -> traverse parseJsonValue tupleValues >>= (Right . Types.Tuple)
        (JsonTypes.Object o) ->
            let (keys, values) = unzip $ HashMap.toList $ HashMap.map parseJsonValue o
            in case List.filter (not . startsWithLowercaseLetter) keys of
                   [] -> fmap (Types.Record . HashMap.fromList . zip keys) (sequence values)
                   invalidKeys -> Left (InvalidRecordKeys invalidKeys)
        invalid -> Left $ InvalidType invalid

startsWithLowercaseLetter :: Text.Text -> Bool
startsWithLowercaseLetter = maybe False (Char.isLower . fst) . Text.uncons

displayParseError :: ParseError -> Text.Text
displayParseError err =
    case err of
        UnknownType invalidType ->
            "I was expecting one of \"" <> intJsonValue <> "\", \"" <> floatJsonValue <> "\", \"" <>
            boolJsonValue <>
            "\", \"" <>
            stringJsonValue <>
            "\" or an object, but got \"" <>
            invalidType <>
            "\" instead."
        InvalidRecordKeys invalidKeys ->
            "I was each key on a record to start with a lowercase letter, but I got the following keys: " <>
            (Text.intercalate ", " $ List.map (\k -> "\"" <> k <> "\"") invalidKeys) <>
            "."
        InvalidType invalidType ->
            "I was expecting a string or object type, but got " <> PMisc.jsonValueToText invalidType <>
            "."
