{-# LANGUAGE OverloadedStrings #-}

module Parser.LanguageConfig
    ( parseLanguageConfig
    , languageConfigParser
    , displayLanguageConfigParseError
    , LanguageConfigParseError
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

parseLanguageConfig :: BS.ByteString -> Either String Types.LanguageConfig
parseLanguageConfig = JsonBase.eitherDecode

instance JsonTypes.FromJSON Types.LanguageConfig where
    parseJSON =
        Json.toAesonParser displayLanguageConfigParseError (Json.withObject languageConfigParser)

data LanguageConfigParseError
    = MissingLanguageName
    | InvalidLanguageNameValue Text.Text
    | InvalidLanguageNameType JsonTypes.Value
    | MissingLanguageOutputPath Text.Text
    | InvalidLanguageOutputPathType JsonTypes.Value
                                    Text.Text

languageNameJsonKey :: Text.Text
languageNameJsonKey = "name"

elmLanguageJsonValue :: Text.Text
elmLanguageJsonValue = "elm"

reasonLanguageJsonValue :: Text.Text
reasonLanguageJsonValue = "reason"

languageOutputPathJsonKey :: Text.Text
languageOutputPathJsonKey = "outputPath"

languageConfigParser :: JsonTypes.Object -> Either LanguageConfigParseError Types.LanguageConfig
languageConfigParser o =
    let maybeName = HashMap.lookup languageNameJsonKey o
    in case maybeName of
           Just nameValue ->
               case nameValue of
                   (JsonTypes.String name)
                       | name == elmLanguageJsonValue ->
                           Misc.mapLeft (Misc.apply name) (elmConfigParser o)
                       | name == reasonLanguageJsonValue ->
                           Misc.mapLeft (Misc.apply name) (reasonConfigParser o)
                       | otherwise -> Left (InvalidLanguageNameValue name)
                   invalid -> Left (InvalidLanguageNameType invalid)
           Nothing -> Left MissingLanguageName

elmConfigParser ::
       HashMap.HashMap Text.Text JsonTypes.Value
    -> Either (Text.Text -> LanguageConfigParseError) Types.LanguageConfig
elmConfigParser o =
    let maybeOutputPath = HashMap.lookup languageOutputPathJsonKey o
    in case maybeOutputPath of
           Just outputPathValue ->
               case outputPathValue of
                   (JsonTypes.String outputPath) -> Right (Types.ElmConfig outputPath)
                   invalid -> Left (InvalidLanguageOutputPathType invalid)
           Nothing -> Left MissingLanguageOutputPath

reasonConfigParser ::
       HashMap.HashMap Text.Text JsonTypes.Value
    -> Either (Text.Text -> LanguageConfigParseError) Types.LanguageConfig
reasonConfigParser o =
    let maybeOutputPath = HashMap.lookup languageOutputPathJsonKey o
    in case maybeOutputPath of
           Just outputPathValue ->
               case outputPathValue of
                   (JsonTypes.String outputPath) -> Right (Types.ReasonConfig outputPath)
                   invalid -> Left (InvalidLanguageOutputPathType invalid)
           Nothing -> Left MissingLanguageOutputPath

displayLanguageConfigParseError :: LanguageConfigParseError -> Text.Text
displayLanguageConfigParseError err =
    case err of
        MissingLanguageName ->
            "I was expecting the \"name\" field to exists on the language config, but I didn't find it!"
        InvalidLanguageNameType invalidType ->
            "I was expecting the \"name\" field on the language config to be a \"string\" , but it was of type " <>
            PMisc.jsonValueToText invalidType <>
            "."
        InvalidLanguageNameValue invalidValue ->
            "I was expecting the \"name\" field on the language config to be \"" <>
            elmLanguageJsonValue <>
            "\" or \"" <>
            reasonLanguageJsonValue <>
            "\", but it was \"" <>
            invalidValue <>
            "\"."
        MissingLanguageOutputPath languageName ->
            "I was expecting the \"outputPath\" field to exist on the language config for \"" <>
            languageName <>
            "\", but I didn't find it!"
        InvalidLanguageOutputPathType invalidType languageName ->
            "I was expecting the \"outputPath\" field on the language config for \"" <> languageName <>
            "\" to be a \"string\" , but it was of type " <>
            PMisc.jsonValueToText invalidType <>
            "."
