{-# LANGUAGE OverloadedStrings #-}

module Parser.CustomType
    ( parseCustomType
    , customTypeParser
    , displayCustomTypeParserError
    , CustomTypeParseError
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
import qualified Misc
import qualified Parser.Misc as PMisc
import qualified Parser.PrimitiveType as PrimitiveTypeParser
import qualified Types

parseCustomType :: BS.ByteString -> Either String Types.CustomType
parseCustomType = JsonBase.eitherDecode

instance JsonTypes.FromJSON Types.CustomType where
    parseJSON = Json.toAesonParser displayCustomTypeParserError (Json.withObject customTypeParser)

data CustomTypeParseError
    = MissingCustomTypeName
    | InvalidCustomTypeNameType JsonTypes.Value
    | MissingCustomTypeKind
    | InvalidCustomTypeKindType JsonTypes.Value
    | InvalidCustomTypeKindValue Text.Text
    | MissingCustomTypeValue Text.Text
    | MissingCustomTypeConstructors Text.Text
    | CustomTypeAliasValueTypeError PrimitiveTypeParser.TypeParseError
                                    Text.Text
    | CustomTypeConstructorsTypeError JsonTypes.Value
                                      Text.Text
    | CustomTypeConstructorValueTypeError PrimitiveTypeParser.TypeParseError
                                          Text.Text
    deriving (Show)

nameJsonKey :: Text.Text
nameJsonKey = "name"

kindJsonKey :: Text.Text
kindJsonKey = "kind"

kindAliasJsonValue :: Text.Text
kindAliasJsonValue = "alias"

kindUnionJsonValue :: Text.Text
kindUnionJsonValue = "union"

valueJsonKey :: Text.Text
valueJsonKey = "value"

constructorsJsonKey :: Text.Text
constructorsJsonKey = "constructors"

customTypeParser :: JsonTypes.Object -> Either CustomTypeParseError Types.CustomType
customTypeParser o =
    let maybeName = HashMap.lookup nameJsonKey o
        maybeKind = HashMap.lookup kindJsonKey o
    in case (maybeName, maybeKind) of
           (Nothing, _) -> Left MissingCustomTypeName
           (_, Nothing) -> Left MissingCustomTypeKind
           (Just nameJsonValue, Just kindJsonValue) ->
               case (nameJsonValue, kindJsonValue) of
                   (JsonTypes.String name, JsonTypes.String kind)
                       | kind == kindAliasJsonValue ->
                           let eitherAliasTypeValue = aliasTypeValueParser o
                           in Misc.mapLeft (Misc.apply name) $
                              fmap (Types.Alias name) eitherAliasTypeValue
                       | kind == kindUnionJsonValue ->
                           let eitherUnionTypeConstructors = unionTypeConstructorsParser o
                           in Misc.mapLeft (Misc.apply name) $
                              fmap (Types.Union name) eitherUnionTypeConstructors
                       | otherwise -> Left (InvalidCustomTypeKindValue kind)
                   (_, JsonTypes.String _) -> Left (InvalidCustomTypeNameType nameJsonValue)
                   (JsonTypes.String _, _) -> Left (InvalidCustomTypeKindType kindJsonValue)
                   (_, _) -> Left (InvalidCustomTypeNameType nameJsonValue)

aliasTypeValueParser ::
       HashMap.HashMap Text.Text JsonTypes.Value
    -> Either (Text.Text -> CustomTypeParseError) Types.PrimitiveType
aliasTypeValueParser o =
    let maybeValue = HashMap.lookup valueJsonKey o
    in case maybeValue of
           Just value ->
               Misc.mapLeft
                   CustomTypeAliasValueTypeError
                   (PrimitiveTypeParser.primitiveTypeParser value)
           Nothing -> Left MissingCustomTypeValue

unionTypeConstructorsParser ::
       HashMap.HashMap Text.Text JsonTypes.Value
    -> Either (Text.Text -> CustomTypeParseError) (HashMap.HashMap Text.Text [Types.PrimitiveType])
unionTypeConstructorsParser o =
    let maybeConstructors = HashMap.lookup constructorsJsonKey o
    in case maybeConstructors of
           Just constructorValue ->
               case constructorValue of
                   JsonTypes.Object constructorsMap ->
                       traverse primitiveTypeArrayParser constructorsMap
                   invalid -> Left (CustomTypeConstructorsTypeError invalid)
           Nothing -> Left MissingCustomTypeConstructors

primitiveTypeArrayParser ::
       JsonTypes.Value -> Either (Text.Text -> CustomTypeParseError) [Types.PrimitiveType]
primitiveTypeArrayParser value =
    case value of
        JsonTypes.Array vector ->
            (Misc.mapLeft CustomTypeConstructorValueTypeError .
             traverse PrimitiveTypeParser.primitiveTypeParser . Vector.toList)
                vector
        invalid -> Left (CustomTypeConstructorsTypeError invalid)

displayCustomTypeParserError :: CustomTypeParseError -> Text.Text
displayCustomTypeParserError err =
    case err of
        MissingCustomTypeName ->
            "I was expecting the \"name\" field to exist on the alias/union type, but I didn't find it!"
        InvalidCustomTypeNameType invalidType ->
            "I was expecting the \"name\" field on the alias/union type to be of type \"string\", but it was " <>
            PMisc.jsonValueToText invalidType <>
            "."
        MissingCustomTypeKind ->
            "I was expecting the \"kind\" field to exist on the alias/union type, but I didn't find it!"
        InvalidCustomTypeKindType invalidType ->
            "I was expecting the \"kind\" field on the alias/union type to be of type \"string\", but it was " <>
            PMisc.jsonValueToText invalidType <>
            "."
        InvalidCustomTypeKindValue invalidKind ->
            "I was expecting the \"kind\" field on the alias/union type to be either \"alias\" or \"union\", but it was \"" <>
            invalidKind <>
            "\"."
        MissingCustomTypeValue aliasName ->
            "I was expecting the \"value\" field to exist on the alias type \"" <> aliasName <>
            "\", but I didn't find it!"
        MissingCustomTypeConstructors unionName ->
            "I was expecting the \"constructors\" field to exist on the union type \"" <> unionName <>
            "\", but I didn't find it!"
        CustomTypeAliasValueTypeError err typeName ->
            "There's an issue in the alias type \"" <> typeName <> "\". " <>
            PrimitiveTypeParser.displayPrimitiveTypeParserError err
        CustomTypeConstructorsTypeError invalidType typeName ->
            "On the \"" <> typeName <>
            "\" union type, I was expecting the \"constructors\" field to be a record, but it was of type " <>
            PMisc.jsonValueToText invalidType <>
            "."
        CustomTypeConstructorValueTypeError err typeName ->
            "There's an issue with one of the constructor values on the union type \"" <> typeName <>
            "\". " <>
            PrimitiveTypeParser.displayPrimitiveTypeParserError err
