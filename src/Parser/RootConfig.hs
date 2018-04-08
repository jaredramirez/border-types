{-# LANGUAGE OverloadedStrings #-}

module Parser.RootConfig
    ( parseConfig
    , configParser
    , displayConfigParseError
    , ConfigParseError
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
import qualified Parser.CustomType as CustomTypeParser
import qualified Parser.LanguageConfig as LanguageConfigParser
import qualified Parser.Misc as PMisc
import qualified Types

parseConfig :: BS.ByteString -> Either String Types.Config
parseConfig = JsonBase.eitherDecode

instance JsonTypes.FromJSON Types.Config where
    parseJSON = Json.toAesonParser displayConfigParseError (Json.withObject configParser)

data ConfigParseError
    = MissingConfigLangauges
    | InvalidConfigLangaugesType JsonTypes.Value
    | InvalidConfigLangaugesChildType JsonTypes.Value
    | InvalidConfigLangauges LanguageConfigParser.LanguageConfigParseError
    | MissingConfigTypes
    | InvalidConfigTypesType JsonTypes.Value
    | InvalidConfigTypesChildType JsonTypes.Value
    | InvalidConfigTypes CustomTypeParser.CustomTypeParseError

configLanguagesJsonKey :: Text.Text
configLanguagesJsonKey = "languages"

configTypesJsonKey :: Text.Text
configTypesJsonKey = "types"

configParser :: JsonTypes.Object -> Either ConfigParseError Types.Config
configParser o =
    let maybeLanguages = HashMap.lookup configLanguagesJsonKey o
        maybeTypes = HashMap.lookup configTypesJsonKey o
    in case (maybeLanguages, maybeTypes) of
           (Just languagesValue, Just typesValue) ->
               let eitherLanguages = languagesParser languagesValue
                   eitherTypes = typesParser typesValue
               in Types.Config <$> eitherLanguages <*> eitherTypes
           (Nothing, _) -> Left MissingConfigLangauges
           (_, Nothing) -> Left MissingConfigTypes

languagesParser :: JsonTypes.Value -> Either ConfigParseError [Types.LanguageConfig]
languagesParser value =
    case value of
        (JsonTypes.Array v) ->
            let objectsList = traverse (toObject InvalidConfigLangaugesChildType) $ Vector.toList v
            in objectsList >>=
               (Misc.mapLeft InvalidConfigLangauges .
                traverse LanguageConfigParser.languageConfigParser)
        invalid -> Left (InvalidConfigLangaugesType invalid)

typesParser :: JsonTypes.Value -> Either ConfigParseError [Types.CustomType]
typesParser value =
    case value of
        (JsonTypes.Array v) ->
            let objectsList = traverse (toObject InvalidConfigTypesChildType) $ Vector.toList v
            in objectsList >>=
               (Misc.mapLeft InvalidConfigTypes . traverse CustomTypeParser.customTypeParser)
        invalid -> Left (InvalidConfigTypesType invalid)

toObject ::
       (JsonTypes.Value -> ConfigParseError)
    -> JsonTypes.Value
    -> Either ConfigParseError JsonTypes.Object
toObject toError value =
    case value of
        (JsonTypes.Object o) -> Right o
        invalid -> Left (toError invalid)

displayConfigParseError :: ConfigParseError -> Text.Text
displayConfigParseError err =
    case err of
        MissingConfigLangauges ->
            "I was expecting the \"langauges\" field to exists on the root config, but I didn't find it!"
        InvalidConfigLangaugesType invalidType ->
            "I was expecting the \"langauges\" field to be an array, but it was of type " <>
            PMisc.jsonValueToText invalidType <>
            "."
        InvalidConfigLangaugesChildType invalidChildTypes ->
            "I was expecting the \"langauges\" field to be an array of language config objecs, but one of the children was of type " <>
            PMisc.jsonValueToText invalidChildTypes <>
            "."
        InvalidConfigLangauges childError ->
            "There's an issue with one the language config objects. " <>
            LanguageConfigParser.displayLanguageConfigParseError childError
        MissingConfigTypes ->
            "I was expecting the \"types\" field to exists on the root config, but I didn't find it!"
        InvalidConfigTypesType invalidType ->
            "I was expecting the \"types\" field to be an array, but it was of type " <>
            PMisc.jsonValueToText invalidType <>
            "."
        InvalidConfigTypesChildType invalidChildTypes ->
            "I was expecting the \"types\" field to be an array of type definitons, but one of the children was of type " <>
            PMisc.jsonValueToText invalidChildTypes <>
            "."
        InvalidConfigTypes childError ->
            "There's an issue with one the type definitions in the \"types\" field. " <>
            CustomTypeParser.displayCustomTypeParserError childError
