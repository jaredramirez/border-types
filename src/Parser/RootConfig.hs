{-# LANGUAGE OverloadedStrings #-}

module Parser.RootConfig
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
import qualified Parser.CustomType as CustomTypeParser
import qualified Parser.LanguageConfig as LanguageConfigParser
import qualified Parser.Misc as PMisc
import qualified Types

parseString :: BS.ByteString -> Either String Types.RootConfig
parseString = JsonBase.eitherDecode

instance JsonTypes.FromJSON Types.RootConfig where
    parseJSON = Json.toAesonParser displayParseError (Json.withObject parseJsonObject)

data ParseError
    = MissingLangauges
    | InvalidLangaugesType JsonTypes.Value
    | InvalidLangaugesChildType JsonTypes.Value
    | InvalidLangauges LanguageConfigParser.ParseError
    | MissingTypes
    | InvalidTypesType JsonTypes.Value
    | InvalidTypesChildType JsonTypes.Value
    | InvalidTypes CustomTypeParser.ParseError

languagesJsonKey :: Text.Text
languagesJsonKey = "languages"

typesJsonKey :: Text.Text
typesJsonKey = "types"

parseJsonObject :: JsonTypes.Object -> Either ParseError Types.RootConfig
parseJsonObject o =
    let maybeLanguagesValue = HashMap.lookup languagesJsonKey o
        maybeTypesValue = HashMap.lookup typesJsonKey o
    in case (maybeLanguagesValue, maybeTypesValue) of
           (Just languagesValue, Just typesValue) ->
               let eitherLanguages = languagesParser languagesValue
                   eitherTypes = typesParser typesValue
               in Types.RootConfig <$> eitherLanguages <*> eitherTypes
           (Nothing, _) -> Left MissingLangauges
           (_, Nothing) -> Left MissingTypes

languagesParser :: JsonTypes.Value -> Either ParseError [Types.LanguageConfig]
languagesParser value =
    case value of
        (JsonTypes.Array v) ->
            let objectsList = traverse (toObject InvalidLangaugesChildType) $ Vector.toList v
            in objectsList >>=
               (Misc.mapLeft InvalidLangauges . traverse LanguageConfigParser.parseJsonObject)
        invalid -> Left (InvalidLangaugesType invalid)

typesParser :: JsonTypes.Value -> Either ParseError [Types.CustomType]
typesParser value =
    case value of
        (JsonTypes.Array v) ->
            let objectsList = traverse (toObject InvalidTypesChildType) $ Vector.toList v
            in objectsList >>=
               (Misc.mapLeft InvalidTypes . traverse CustomTypeParser.parseJsonObject)
        invalid -> Left (InvalidTypesType invalid)

toObject :: (JsonTypes.Value -> ParseError) -> JsonTypes.Value -> Either ParseError JsonTypes.Object
toObject toError value =
    case value of
        (JsonTypes.Object o) -> Right o
        invalid -> Left (toError invalid)

displayParseError :: ParseError -> Text.Text
displayParseError err =
    case err of
        MissingLangauges ->
            "I was expecting the \"langauges\" field to exists on the root config, but I didn't find it!"
        InvalidLangaugesType invalidType ->
            "I was expecting the \"langauges\" field to be an array, but it was of type " <>
            PMisc.jsonValueToText invalidType <>
            "."
        InvalidLangaugesChildType invalidChildTypes ->
            "I was expecting the \"langauges\" field to be an array of language config objecs, but one of the children was of type " <>
            PMisc.jsonValueToText invalidChildTypes <>
            "."
        InvalidLangauges childError ->
            "There's an issue with one the language config objects. " <>
            LanguageConfigParser.displayParseError childError
        MissingTypes ->
            "I was expecting the \"types\" field to exists on the root config, but I didn't find it!"
        InvalidTypesType invalidType ->
            "I was expecting the \"types\" field to be an array, but it was of type " <>
            PMisc.jsonValueToText invalidType <>
            "."
        InvalidTypesChildType invalidChildTypes ->
            "I was expecting the \"types\" field to be an array of type definitons, but one of the children was of type " <>
            PMisc.jsonValueToText invalidChildTypes <>
            "."
        InvalidTypes childError ->
            "There's an issue with one the type definitions in the \"types\" field. " <>
            CustomTypeParser.displayParseError childError
