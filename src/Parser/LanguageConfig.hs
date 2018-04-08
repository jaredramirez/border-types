{-# LANGUAGE OverloadedStrings #-}

module Parser.LanguageConfig
    ( parseString
    , parseJsonObject
    , displayParseError
    , ParseError
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
import qualified Types

parseString :: BS.ByteString -> Either String Types.LanguageConfig
parseString = JsonBase.eitherDecode

instance JsonTypes.FromJSON Types.LanguageConfig where
    parseJSON = Json.toAesonParser displayParseError (Json.withObject parseJsonObject)

data ParseError
    = MissingName
    | InvalidNameValue Text.Text
    | InvalidNameType JsonTypes.Value
    | MissingOutputPath Text.Text
    | InvalidOutputPathType JsonTypes.Value
                            Text.Text

nameJsonKey :: Text.Text
nameJsonKey = "name"

elmJsonValue :: Text.Text
elmJsonValue = "elm"

reasonJsonValue :: Text.Text
reasonJsonValue = "reason"

outputPathJsonKey :: Text.Text
outputPathJsonKey = "outputPath"

parseJsonObject :: JsonTypes.Object -> Either ParseError Types.LanguageConfig
parseJsonObject o =
    let maybeName = HashMap.lookup nameJsonKey o
    in case maybeName of
           Just nameValue ->
               case nameValue of
                   (JsonTypes.String name)
                       | name == elmJsonValue -> Misc.mapLeft (Misc.apply name) (elmConfigParser o)
                       | name == reasonJsonValue ->
                           Misc.mapLeft (Misc.apply name) (reasonConfigParser o)
                       | otherwise -> Left (InvalidNameValue name)
                   invalid -> Left (InvalidNameType invalid)
           Nothing -> Left MissingName

elmConfigParser ::
       HashMap.HashMap Text.Text JsonTypes.Value
    -> Either (Text.Text -> ParseError) Types.LanguageConfig
elmConfigParser o =
    let maybeOutputPath = HashMap.lookup outputPathJsonKey o
    in case maybeOutputPath of
           Just outputPathValue ->
               case outputPathValue of
                   (JsonTypes.String outputPath) -> Right (Types.ElmConfig outputPath)
                   invalid -> Left (InvalidOutputPathType invalid)
           Nothing -> Left MissingOutputPath

reasonConfigParser ::
       HashMap.HashMap Text.Text JsonTypes.Value
    -> Either (Text.Text -> ParseError) Types.LanguageConfig
reasonConfigParser o =
    let maybeOutputPath = HashMap.lookup outputPathJsonKey o
    in case maybeOutputPath of
           Just outputPathValue ->
               case outputPathValue of
                   (JsonTypes.String outputPath) -> Right (Types.ReasonConfig outputPath)
                   invalid -> Left (InvalidOutputPathType invalid)
           Nothing -> Left MissingOutputPath

displayParseError :: ParseError -> Text.Text
displayParseError err =
    case err of
        MissingName ->
            "I was expecting the \"name\" field to exists on the language config, but I didn't find it!"
        InvalidNameType invalidType ->
            "I was expecting the \"name\" field on the language config to be a \"string\" , but it was of type " <>
            PMisc.jsonValueToText invalidType <>
            "."
        InvalidNameValue invalidValue ->
            "I was expecting the \"name\" field on the language config to be \"" <> elmJsonValue <>
            "\" or \"" <>
            reasonJsonValue <>
            "\", but it was \"" <>
            invalidValue <>
            "\"."
        MissingOutputPath languageName ->
            "I was expecting the \"outputPath\" field to exist on the language config for \"" <>
            languageName <>
            "\", but I didn't find it!"
        InvalidOutputPathType invalidType languageName ->
            "I was expecting the \"outputPath\" field on the language config for \"" <> languageName <>
            "\" to be a \"string\" , but it was of type " <>
            PMisc.jsonValueToText invalidType <>
            "."
