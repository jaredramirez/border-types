{-# LANGUAGE OverloadedStrings #-}

module Ast.Parse
    ( parseConfig
    , parseLanguageConfig
    , parseCustomType
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

-- CONFIG
parseConfig :: BS.ByteString -> Either String AST.Config
parseConfig = JsonBase.eitherDecode

instance JsonTypes.FromJSON AST.Config where
    parseJSON = Json.toAesonParser displayConfigParseError (Json.withObject configParser)

data ConfigParseError
    = MissingConfigLangauges
    | InvalidConfigLangaugesType JsonTypes.Value
    | InvalidConfigLangaugesChildType JsonTypes.Value
    | InvalidConfigLangauges LanguageConfigParseError
    | MissingConfigTypes
    | InvalidConfigTypesType JsonTypes.Value
    | InvalidConfigTypesChildType JsonTypes.Value
    | InvalidConfigTypes CustomTypeParseError

configLanguagesJsonKey :: Text.Text
configLanguagesJsonKey = "languages"

configTypesJsonKey :: Text.Text
configTypesJsonKey = "types"

configParser :: JsonTypes.Object -> Either ConfigParseError AST.Config
configParser o =
    let maybeLanguages = HashMap.lookup configLanguagesJsonKey o
        maybeTypes = HashMap.lookup configTypesJsonKey o
    in case (maybeLanguages, maybeTypes) of
           (Just languagesValue, Just typesValue) ->
               let eitherLanguages = languagesParser languagesValue
                   eitherTypes = typesParser typesValue
               in AST.Config <$> eitherLanguages <*> eitherTypes
           (Nothing, _) -> Left MissingConfigLangauges
           (_, Nothing) -> Left MissingConfigTypes

languagesParser :: JsonTypes.Value -> Either ConfigParseError [AST.LanguageConfig]
languagesParser value =
    case value of
        (JsonTypes.Array v) ->
            let objectsList = traverse (toObject InvalidConfigLangaugesChildType) $ Vector.toList v
            in objectsList >>= (Misc.mapLeft InvalidConfigLangauges . traverse languageConfigParser)
        invalid -> Left (InvalidConfigLangaugesType invalid)

typesParser :: JsonTypes.Value -> Either ConfigParseError [AST.CustomType]
typesParser value =
    case value of
        (JsonTypes.Array v) ->
            let objectsList = traverse (toObject InvalidConfigTypesChildType) $ Vector.toList v
            in objectsList >>= (Misc.mapLeft InvalidConfigTypes . traverse customTypeParser)
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
            jsonValueToText invalidType <>
            "."
        InvalidConfigLangaugesChildType invalidChildTypes ->
            "I was expecting the \"langauges\" field to be an array of language config objecs, but one of the children was of type " <>
            jsonValueToText invalidChildTypes <>
            "."
        InvalidConfigLangauges childError ->
            "There's an issue with one the language config objects. " <>
            displayLanguageConfigParseError childError
        MissingConfigTypes ->
            "I was expecting the \"types\" field to exists on the root config, but I didn't find it!"
        InvalidConfigTypesType invalidType ->
            "I was expecting the \"types\" field to be an array, but it was of type " <>
            jsonValueToText invalidType <>
            "."
        InvalidConfigTypesChildType invalidChildTypes ->
            "I was expecting the \"types\" field to be an array of type definitons, but one of the children was of type " <>
            jsonValueToText invalidChildTypes <>
            "."
        InvalidConfigTypes childError ->
            "There's an issue with one the type definitions in the \"types\" field. " <>
            displayCustomTypeParserError childError

-- LANGUAGE CONFIG
parseLanguageConfig :: BS.ByteString -> Either String AST.LanguageConfig
parseLanguageConfig = JsonBase.eitherDecode

instance JsonTypes.FromJSON AST.LanguageConfig where
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

languageConfigParser :: JsonTypes.Object -> Either LanguageConfigParseError AST.LanguageConfig
languageConfigParser o =
    let maybeName = HashMap.lookup nameJsonKey o
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
    -> Either (Text.Text -> LanguageConfigParseError) AST.LanguageConfig
elmConfigParser o =
    let maybeOutputPath = HashMap.lookup languageOutputPathJsonKey o
    in case maybeOutputPath of
           Just outputPathValue ->
               case outputPathValue of
                   (JsonTypes.String outputPath) -> Right (AST.ElmConfig outputPath)
                   invalid -> Left (InvalidLanguageOutputPathType invalid)
           Nothing -> Left MissingLanguageOutputPath

reasonConfigParser ::
       HashMap.HashMap Text.Text JsonTypes.Value
    -> Either (Text.Text -> LanguageConfigParseError) AST.LanguageConfig
reasonConfigParser o =
    let maybeOutputPath = HashMap.lookup languageOutputPathJsonKey o
    in case maybeOutputPath of
           Just outputPathValue ->
               case outputPathValue of
                   (JsonTypes.String outputPath) -> Right (AST.ReasonConfig outputPath)
                   invalid -> Left (InvalidLanguageOutputPathType invalid)
           Nothing -> Left MissingLanguageOutputPath

displayLanguageConfigParseError :: LanguageConfigParseError -> Text.Text
displayLanguageConfigParseError err =
    case err of
        MissingLanguageName ->
            "I was expecting the \"name\" field to exists on the language config, but I didn't find it!"
        InvalidLanguageNameType invalidType ->
            "I was expecting the \"name\" field on the language config to be a \"string\" , but it was of type " <>
            jsonValueToText invalidType <>
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
            jsonValueToText invalidType <>
            "."

-- CUSTOM TYPE
parseCustomType :: BS.ByteString -> Either String AST.CustomType
parseCustomType = JsonBase.eitherDecode

instance JsonTypes.FromJSON AST.CustomType where
    parseJSON = Json.toAesonParser displayCustomTypeParserError (Json.withObject customTypeParser)

data CustomTypeParseError
    = MissingCustomTypeName
    | InvalidCustomTypeNameType JsonTypes.Value
    | MissingCustomTypeKind
    | InvalidCustomTypeKindType JsonTypes.Value
    | InvalidCustomTypeKindValue Text.Text
    | MissingCustomTypeValue Text.Text
    | MissingCustomTypeConstructors Text.Text
    | CustomTypeAliasValueTypeError TypeParseError
                                    Text.Text
    | CustomTypeConstructorsTypeError JsonTypes.Value
                                      Text.Text
    | CustomTypeConstructorValueTypeError TypeParseError
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
           (Nothing, _) -> Left MissingCustomTypeName
           (_, Nothing) -> Left MissingCustomTypeKind
           (Just nameJsonValue, Just kindJsonValue) ->
               case (nameJsonValue, kindJsonValue) of
                   (JsonTypes.String name, JsonTypes.String kind)
                       | kind == kindAliasJsonValue ->
                           let eitherAliasTypeValue = aliasTypeValueParser o
                           in Misc.mapLeft (Misc.apply name) $
                              fmap (AST.Alias name) eitherAliasTypeValue
                       | kind == kindUnionJsonValue ->
                           let eitherUnionTypeConstructors = unionTypeConstructorsParser o
                           in Misc.mapLeft (Misc.apply name) $
                              fmap (AST.Union name) eitherUnionTypeConstructors
                       | otherwise -> Left (InvalidCustomTypeKindValue kind)
                   (_, JsonTypes.String _) -> Left (InvalidCustomTypeNameType nameJsonValue)
                   (JsonTypes.String _, _) -> Left (InvalidCustomTypeKindType kindJsonValue)
                   (_, _) -> Left (InvalidCustomTypeNameType nameJsonValue)

aliasTypeValueParser ::
       HashMap.HashMap Text.Text JsonTypes.Value
    -> Either (Text.Text -> CustomTypeParseError) AST.PrimitiveType
aliasTypeValueParser o =
    let maybeValue = HashMap.lookup valueJsonKey o
    in case maybeValue of
           Just value -> Misc.mapLeft CustomTypeAliasValueTypeError (primitiveTypeParser value)
           Nothing -> Left MissingCustomTypeValue

unionTypeConstructorsParser ::
       HashMap.HashMap Text.Text JsonTypes.Value
    -> Either (Text.Text -> CustomTypeParseError) (HashMap.HashMap Text.Text [AST.PrimitiveType])
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
       JsonTypes.Value -> Either (Text.Text -> CustomTypeParseError) [AST.PrimitiveType]
primitiveTypeArrayParser value =
    case value of
        JsonTypes.Array vector ->
            (Misc.mapLeft CustomTypeConstructorValueTypeError .
             traverse primitiveTypeParser . Vector.toList)
                vector
        invalid -> Left (CustomTypeConstructorsTypeError invalid)

displayCustomTypeParserError :: CustomTypeParseError -> Text.Text
displayCustomTypeParserError err =
    case err of
        MissingCustomTypeName ->
            "I was expecting the \"name\" field to exist on the alias/union type, but I didn't find it!"
        InvalidCustomTypeNameType invalidType ->
            "I was expecting the \"name\" field on the alias/union type to be of type \"string\", but it was " <>
            jsonValueToText invalidType <>
            "."
        MissingCustomTypeKind ->
            "I was expecting the \"kind\" field to exist on the alias/union type, but I didn't find it!"
        InvalidCustomTypeKindType invalidType ->
            "I was expecting the \"kind\" field on the alias/union type to be of type \"string\", but it was " <>
            jsonValueToText invalidType <>
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
            displayPrimitiveTypeParserError err
        CustomTypeConstructorsTypeError invalidType typeName ->
            "On the \"" <> typeName <>
            "\" union type, I was expecting the \"constructors\" field to be a record, but it was of type " <>
            jsonValueToText invalidType <>
            "."
        CustomTypeConstructorValueTypeError err typeName ->
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
        (JsonTypes.Array v) ->
            let list = Vector.toList v
            in case list of
                   [] -> Right AST.Unit
                   [listValue] -> primitiveTypeParser listValue >>= (Right . AST.List)
                   tupleValues -> traverse primitiveTypeParser tupleValues >>= (Right . AST.Tuple)
        (JsonTypes.Object o) ->
            let (keys, values) = unzip $ HashMap.toList $ HashMap.map primitiveTypeParser o
                map = fmap (AST.Record . HashMap.fromList . zip keys) (sequence values)
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
            "I was expecting a string or object type, but got " <> jsonValueToText invalidType <>
            "."

-- OTHER
jsonValueToText :: JsonTypes.Value -> Text.Text
jsonValueToText = Text.drop 2 . Text.pack . show . Json.jsonTypeOf
