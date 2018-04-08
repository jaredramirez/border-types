{-# LANGUAGE OverloadedStrings #-}

module Parser.PrimitiveType
    ( parsePrimitiveType
    , primitiveTypeParser
    , displayPrimitiveTypeParserError
    , TypeParseError
    ) where

import qualified Data.Aeson as JsonBase
import qualified Data.Aeson.BetterErrors as Json
import qualified Data.Aeson.Types as JsonTypes
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Parser.Misc as PMisc
import qualified Types

parsePrimitiveType :: BS.ByteString -> Either String Types.PrimitiveType
parsePrimitiveType = JsonBase.eitherDecode

instance JsonTypes.FromJSON Types.PrimitiveType where
    parseJSON =
        Json.toAesonParser displayPrimitiveTypeParserError (Json.withValue primitiveTypeParser)

data TypeParseError
    = UnknownPrimitiveType Text.Text
    | UnknownPrimitiveTypeWithGuess Text.Text
                                    Text.Text
    | InvalidPrimitiveType JsonTypes.Value
    deriving (Show)

intJsonKey :: Text.Text
intJsonKey = "int"

floatJsonKey :: Text.Text
floatJsonKey = "float"

boolJsonKey :: Text.Text
boolJsonKey = "bool"

stringJsonKey :: Text.Text
stringJsonKey = "string"

-- This method is pretty primative, it should probably be improved
typeMisspellings :: [(Text.Text, Text.Text)]
typeMisspellings =
    [("Int", intJsonKey), ("Float", floatJsonKey), ("Bool", boolJsonKey), ("String", stringJsonKey)]

primitiveTypeParser :: JsonTypes.Value -> Either TypeParseError Types.PrimitiveType
primitiveTypeParser value =
    case value of
        (JsonTypes.String s)
            | s == intJsonKey -> Right Types.Int
            | s == floatJsonKey -> Right Types.Float
            | s == boolJsonKey -> Right Types.Bool
            | s == stringJsonKey -> Right Types.String
            | otherwise ->
                case List.find ((==) s . fst) typeMisspellings of
                    Just (_, guess) -> Left $ UnknownPrimitiveTypeWithGuess s guess
                    Nothing -> Left $ UnknownPrimitiveType s
        (JsonTypes.Array v) ->
            let list = Vector.toList v
            in case list of
                   [] -> Right Types.Unit
                   [listValue] -> primitiveTypeParser listValue >>= (Right . Types.List)
                   tupleValues -> traverse primitiveTypeParser tupleValues >>= (Right . Types.Tuple)
        (JsonTypes.Object o) ->
            let (keys, values) = unzip $ HashMap.toList $ HashMap.map primitiveTypeParser o
                map = fmap (Types.Record . HashMap.fromList . zip keys) (sequence values)
            in map
        invalid -> Left $ InvalidPrimitiveType invalid

displayPrimitiveTypeParserError :: TypeParseError -> Text.Text
displayPrimitiveTypeParserError err =
    case err of
        UnknownPrimitiveType invalidType ->
            "I was expecting one of \"" <> intJsonKey <> "\", \"" <> floatJsonKey <> "\", \"" <>
            boolJsonKey <>
            "\", \"" <>
            stringJsonKey <>
            "\" or an object, but got \"" <>
            invalidType <>
            "\" instead."
        UnknownPrimitiveTypeWithGuess invalidType guess ->
            "I got \"" <> invalidType <> "\" as a field type, which is invalid. Did you mean \"" <>
            guess <>
            "\"?"
        InvalidPrimitiveType invalidType ->
            "I was expecting a string or object type, but got " <> PMisc.jsonValueToText invalidType <>
            "."
