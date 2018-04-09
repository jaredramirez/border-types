{-# LANGUAGE OverloadedStrings #-}

module Parser.CustomType
  ( parseString
  , parseJsonObject
  , displayParseError
  , ParseError
  ) where

import qualified Data.Aeson              as JsonBase
import qualified Data.Aeson.BetterErrors as Json
import qualified Data.Aeson.Types        as JsonTypes
import qualified Data.ByteString.Lazy    as BS
import qualified Data.Char               as Char
import qualified Data.HashMap.Strict     as HashMap
import qualified Data.List               as List
import           Data.Semigroup          ((<>))
import qualified Data.Text               as Text
import qualified Data.Vector             as Vector
import qualified Misc
import qualified Parser.Misc             as PMisc
import qualified Parser.PrimitiveType    as PrimitiveTypeParser
import qualified Types

parseString :: BS.ByteString -> Either String Types.CustomType
parseString = JsonBase.eitherDecode

instance JsonTypes.FromJSON Types.CustomType where
  parseJSON =
    Json.toAesonParser displayParseError (Json.withObject parseJsonObject)

data ParseError
  = MissingName
  | InvalidNameType JsonTypes.Value
  | InvalidNameValue Text.Text
                     Text.Text
  | MissingKind
  | InvalidKindType JsonTypes.Value
  | InvalidKindValue Text.Text
  | MissingValue Text.Text
  | MissingConstructors Text.Text
  | AliasValueTypeError PrimitiveTypeParser.ParseError
                        Text.Text
  | ConstructorsTypeError JsonTypes.Value
                          Text.Text
  | ConstructorValueTypeError PrimitiveTypeParser.ParseError
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

parseJsonObject :: JsonTypes.Object -> Either ParseError Types.CustomType
parseJsonObject o =
  let maybeName = HashMap.lookup nameJsonKey o
      maybeKind = HashMap.lookup kindJsonKey o
  in case (maybeName, maybeKind) of
       (Nothing, _) -> Left MissingName
       (_, Nothing) -> Left MissingKind
       (Just nameJsonValue, Just kindJsonValue) ->
         case (nameJsonValue, kindJsonValue) of
           (JsonTypes.String name, JsonTypes.String kind)
             | Text.null name ->
               Left (InvalidNameValue "not be an empty string" name)
             | not $ startsWithCapitalLetter name ->
               Left (InvalidNameValue "start with a capital letter" name)
             | containsSpace name ->
               Left (InvalidNameValue "not contain any spaces" name)
             | kind == kindAliasJsonValue ->
               let eitherAliasTypeValue = aliasTypeValueParser o
               in Misc.mapLeft (Misc.apply name) $
                  fmap (Types.Alias name) eitherAliasTypeValue
             | kind == kindUnionJsonValue ->
               let eitherUnionTypeConstructors = unionTypeConstructorsParser o
               in Misc.mapLeft (Misc.apply name) $
                  fmap (Types.Union name) eitherUnionTypeConstructors
             | otherwise -> Left (InvalidKindValue kind)
           (_, JsonTypes.String _) -> Left (InvalidNameType nameJsonValue)
           (JsonTypes.String _, _) -> Left (InvalidKindType kindJsonValue)
           (_, _) -> Left (InvalidNameType nameJsonValue)

startsWithCapitalLetter :: Text.Text -> Bool
startsWithCapitalLetter = maybe False (Char.isUpper . fst) . Text.uncons

containsSpace :: Text.Text -> Bool
containsSpace = Text.isInfixOf " "

aliasTypeValueParser ::
     HashMap.HashMap Text.Text JsonTypes.Value
  -> Either (Text.Text -> ParseError) Types.PrimitiveType
aliasTypeValueParser o =
  let maybeValue = HashMap.lookup valueJsonKey o
  in case maybeValue of
       Just value ->
         Misc.mapLeft
           AliasValueTypeError
           (PrimitiveTypeParser.parseJsonValue value)
       Nothing -> Left MissingValue

unionTypeConstructorsParser ::
     HashMap.HashMap Text.Text JsonTypes.Value
  -> Either (Text.Text -> ParseError) (HashMap.HashMap Text.Text [Types.PrimitiveType])
unionTypeConstructorsParser o =
  let maybeConstructors = HashMap.lookup constructorsJsonKey o
  in case maybeConstructors of
       Just constructorValue ->
         case constructorValue of
           JsonTypes.Object constructorsMap ->
             traverse primitiveTypeArrayParser constructorsMap
           invalid -> Left (ConstructorsTypeError invalid)
       Nothing -> Left MissingConstructors

primitiveTypeArrayParser ::
     JsonTypes.Value -> Either (Text.Text -> ParseError) [Types.PrimitiveType]
primitiveTypeArrayParser value =
  case value of
    JsonTypes.Array vector ->
      (Misc.mapLeft ConstructorValueTypeError .
       traverse PrimitiveTypeParser.parseJsonValue . Vector.toList)
        vector
    invalid -> Left (ConstructorsTypeError invalid)

displayParseError :: ParseError -> Text.Text
displayParseError err =
  case err of
    MissingName ->
      "I was expecting the \"name\" field to exist on the alias/union type, but I didn't find it!"
    InvalidNameType invalidType ->
      "I was expecting the \"name\" field on the alias/union type to be of type \"string\", but it was " <>
      PMisc.jsonValueToText invalidType <>
      "."
    InvalidNameValue expected got ->
      "I was expecting the \"name\" field on the alias/union type to " <>
      expected <>
      ", but it was \"" <>
      got <>
      "\"."
    MissingKind ->
      "I was expecting the \"kind\" field to exist on the alias/union type, but I didn't find it!"
    InvalidKindType invalidType ->
      "I was expecting the \"kind\" field on the alias/union type to be of type \"string\", but it was " <>
      PMisc.jsonValueToText invalidType <>
      "."
    InvalidKindValue invalidKind ->
      "I was expecting the \"kind\" field on the alias/union type to be either \"alias\" or \"union\", but it was \"" <>
      invalidKind <>
      "\"."
    MissingValue aliasName ->
      "I was expecting the \"value\" field to exist on the alias type \"" <>
      aliasName <>
      "\", but I didn't find it!"
    MissingConstructors unionName ->
      "I was expecting the \"constructors\" field to exist on the union type \"" <>
      unionName <>
      "\", but I didn't find it!"
    AliasValueTypeError err typeName ->
      "There's an issue in the alias type \"" <> typeName <> "\". " <>
      PrimitiveTypeParser.displayParseError err
    ConstructorsTypeError invalidType typeName ->
      "On the \"" <> typeName <>
      "\" union type, I was expecting the \"constructors\" field to be a record, but it was of type " <>
      PMisc.jsonValueToText invalidType <>
      "."
    ConstructorValueTypeError err typeName ->
      "There's an issue with one of the constructor values on the union type \"" <>
      typeName <>
      "\". " <>
      PrimitiveTypeParser.displayParseError err
