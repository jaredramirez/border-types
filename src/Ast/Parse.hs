{-# LANGUAGE OverloadedStrings #-}

module Ast.Parse
    ( parseCustomType
    , parsePrimitiveType
    ) where

import qualified Ast as AST
import qualified Data.Aeson as JsonBase
import qualified Data.Aeson.BetterErrors as Json
import qualified Data.Aeson.Types as JsonTypes
import qualified Data.ByteString.Lazy as BS
import qualified Data.Function
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Semigroup ((<>))
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Misc
import Misc ((|>))

-- CUSTOM TYPE
parseCustomType :: BS.ByteString -> Either String AST.CustomType
parseCustomType = JsonBase.eitherDecode

instance JsonTypes.FromJSON AST.CustomType where
    parseJSON = Json.toAesonParser displayCustomTypeParserError (Json.withObject customTypeParser)

data CustomTypeParseError
    = MissingName
    | InvalidNameType JsonTypes.Value
    | MissingKind
    | InvalidKindType JsonTypes.Value
    | InvalidKindValue Text.Text
    | MissingValue Text.Text
    | MissingConstructors Text.Text
    | AliasValueTypeError TypeParseError
                          Text.Text
    | ConstructorsTypeError JsonTypes.Value
                            Text.Text
    | ConstructorValueTypeError TypeParseError
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

customTypeParser :: JsonTypes.Object -> Either CustomTypeParseError AST.CustomType
customTypeParser o =
    let maybeName = HashMap.lookup nameJsonKey o
        maybeKind = HashMap.lookup kindJsonKey o
    in case (maybeName, maybeKind) of
           (Nothing, _) -> Left MissingName
           (_, Nothing) -> Left MissingKind
           (Just nameJsonValue, Just kindJsonValue) ->
               case (nameJsonValue, kindJsonValue) of
                   (JsonTypes.String name, JsonTypes.String kind)
                       | kind == kindAliasJsonValue ->
                           aliasTypeParser o |> fmap (AST.Alias name) |>
                           Misc.mapLeft (Misc.apply name)
                       | kind == kindUnionJsonValue ->
                           unionTypeParser o |> fmap (AST.Union name) |>
                           Misc.mapLeft (Misc.apply name)
                       | otherwise -> Left (InvalidKindValue kind)
                   (_, JsonTypes.String _) -> Left (InvalidNameType nameJsonValue)
                   (JsonTypes.String _, _) -> Left (InvalidKindType kindJsonValue)
                   (_, _) -> Left (InvalidNameType nameJsonValue)

aliasTypeParser ::
       HashMap.HashMap Text.Text JsonTypes.Value
    -> Either (Text.Text -> CustomTypeParseError) AST.PrimitiveType
aliasTypeParser o =
    let maybeValue = HashMap.lookup valueJsonKey o
    in case maybeValue of
           Just value -> Misc.mapLeft AliasValueTypeError (primitiveTypeParser value)
           Nothing -> Left MissingValue

unionTypeParser ::
       HashMap.HashMap Text.Text JsonTypes.Value
    -> Either (Text.Text -> CustomTypeParseError) (HashMap.HashMap Text.Text [AST.PrimitiveType])
unionTypeParser o =
    let maybeConstructors = HashMap.lookup constructorsJsonKey o
    in case maybeConstructors of
           Just constructorValue ->
               case constructorValue of
                   JsonTypes.Object constructorsMap ->
                       traverse primitiveTypeArrayParser constructorsMap
                   invalid -> Left (ConstructorsTypeError invalid)
           Nothing -> Left MissingConstructors

primitiveTypeArrayParser ::
       JsonTypes.Value -> Either (Text.Text -> CustomTypeParseError) [AST.PrimitiveType]
primitiveTypeArrayParser value =
    case value of
        JsonTypes.Array vector ->
            (Misc.mapLeft ConstructorValueTypeError . traverse primitiveTypeParser . Vector.toList)
                vector
        invalid -> Left (ConstructorsTypeError invalid)

displayCustomTypeParserError :: CustomTypeParseError -> Text.Text
displayCustomTypeParserError err =
    case err of
        MissingName ->
            "I was expecting the \"name\" field to exist on the alias/union type, but I didn't find it!"
        InvalidNameType invalidType ->
            "I was expecting the \"name\" field on the alias/union type to be of type \"string\", but it was " <>
            jsonValueToText invalidType <>
            "."
        MissingKind ->
            "I was expecting the \"kind\" field to exist on the alias/union type, but I didn't find it!"
        InvalidKindType invalidType ->
            "I was expecting the \"kind\" field on the alias/union type to be of type \"string\", but it was " <>
            jsonValueToText invalidType <>
            "."
        InvalidKindValue invalidKind ->
            "I was expecting the \"kind\" field on the alias/union type to be either \"alias\" or \"union\", but it was \"" <>
            invalidKind <>
            "\"."
        MissingValue aliasName ->
            "I was expecting the \"value\" field to exist on the alias type \"" <> aliasName <>
            "\", but I didn't find it!"
        MissingConstructors unionName ->
            "I was expecting the \"constructors\" field to exist on the union type \"" <> unionName <>
            "\", but I didn't find it!"
        AliasValueTypeError err typeName ->
            "There's an issue in the alias type \"" <> typeName <> "\". " <>
            displayPrimitiveTypeParserError err
        ConstructorsTypeError invalidType typeName ->
            "On the \"" <> typeName <>
            "\" union type, I was expecting the \"constructors\" field to be a record, but it was of type " <>
            jsonValueToText invalidType <>
            "."
        ConstructorValueTypeError err typeName ->
            "There's an issue with one of the constructor values on the union type \"" <> typeName <>
            "\". " <>
            displayPrimitiveTypeParserError err

-- BASE TYPE
parsePrimitiveType :: BS.ByteString -> Either String AST.PrimitiveType
parsePrimitiveType = JsonBase.eitherDecode

instance JsonTypes.FromJSON AST.PrimitiveType where
    parseJSON =
        Json.toAesonParser displayPrimitiveTypeParserError (Json.withValue primitiveTypeParser)

data TypeParseError
    = UnknownPrimitiveType Text.Text
    | UnknownPrimitiveTypeWithGuess Text.Text
                                    Text.Text
    | InvalidType JsonTypes.Value
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

primitiveTypeParser :: JsonTypes.Value -> Either TypeParseError AST.PrimitiveType
primitiveTypeParser value =
    case value of
        (JsonTypes.String s)
            | s == intJsonKey -> Right AST.Int
            | s == floatJsonKey -> Right AST.Float
            | s == boolJsonKey -> Right AST.Bool
            | s == stringJsonKey -> Right AST.String
            | otherwise ->
                case List.find ((==) s . fst) typeMisspellings of
                    Just (_, guess) -> Left $ UnknownPrimitiveTypeWithGuess s guess
                    Nothing -> Left $ UnknownPrimitiveType s
        (JsonTypes.Object o) ->
            let (keys, values) = unzip $ HashMap.toList $ HashMap.map primitiveTypeParser o
                map = fmap (AST.Record . HashMap.fromList . zip keys) (sequence values)
            in map
        invalid -> Left $ InvalidType invalid

displayPrimitiveTypeParserError :: TypeParseError -> Text.Text
displayPrimitiveTypeParserError err =
    case err of
        UnknownPrimitiveType invalidType ->
            "I was expecting one of \"int\", \"float\", \"bool\", \"string\" or an object, but got \"" <>
            invalidType <>
            "\" instead."
        UnknownPrimitiveTypeWithGuess invalidType guess ->
            "I got \"" <> invalidType <> "\" as a field type, which is invalid. Did you mean \"" <>
            guess <>
            "\"?"
        InvalidType invalidType ->
            "I was expecting a string or object type, but got " <> jsonValueToText invalidType <>
            "."

-- OTHER
jsonValueToText :: JsonTypes.Value -> Text.Text
jsonValueToText = Text.drop 2 . Text.pack . show . Json.jsonTypeOf
