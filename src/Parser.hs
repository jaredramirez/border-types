{-# LANGUAGE OverloadedStrings #-}

module Parser where

import qualified Data.Aeson as JsonBase
import qualified Data.Aeson.BetterErrors as Json
import qualified Data.Aeson.Types as JsonTypes
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Semigroup ((<>))
import qualified Data.Text as Text

newtype Config =
    Config [CustomType]

data CustomType
    = Alias { name :: String
            , value :: Type }
    | Union { name :: String
            , constructors :: HashMap.HashMap Text.Text Type }
    deriving (Show, Eq)

data CustomTypeError
    = MissingName
    | MissingValue
    | MissingConstructors
    deriving (Show)

aliasJsonKey :: Text.Text
aliasJsonKey = "alias"

unionJsonKey :: Text.Text
unionJsonKey = "union"

nameJsonKey :: Text.Text
nameJsonKey = "name"

valueJsonKey :: Text.Text
valueJsonKey = "value"

constructorsJsonKey :: Text.Text
constructorsJsonKey = "constructors"

-- TYPE PRIMITIVE
data Type
    = String
    | Int
    | Float
    | Bool
    | Record (HashMap.HashMap Text.Text Type)
    deriving (Show, Eq)

instance JsonTypes.FromJSON Type where
    parseJSON = Json.toAesonParser displayTypeParserError (Json.withValue parseType)

data TypeParseError
    = UnknownType Text.Text
    | UnknownTypeWithGuess Text.Text
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

parseType :: JsonTypes.Value -> Either TypeParseError Type
parseType value =
    case value of
        (JsonTypes.String s)
            | s == intJsonKey -> Right Int
            | s == floatJsonKey -> Right Float
            | s == boolJsonKey -> Right Bool
            | s == stringJsonKey -> Right String
            | otherwise ->
                case List.find ((==) s . fst) typeMisspellings of
                    Just (_, guess) -> Left $ UnknownTypeWithGuess s guess
                    Nothing -> Left $ UnknownType s
        (JsonTypes.Object o) ->
            let (keys, values) = unzip $ HashMap.toList $ HashMap.map parseType o
                map = fmap (Record . HashMap.fromList . zip keys) (sequence values)
            in map
        invalid -> Left $ InvalidType invalid

displayTypeParserError :: TypeParseError -> Text.Text
displayTypeParserError err =
    case err of
        UnknownType invalidType ->
            "Uh oh, I was expecting one of \"int\", \"float\", \"bool\", \"string\" or an object, but got \"" <>
            invalidType <>
            "\" instead."
        UnknownTypeWithGuess invalidType guess ->
            "Uh oh, I got \"" <> invalidType <>
            "\" as a field type, which is invalid. Did you mean \"" <>
            guess <>
            "\"?"
        InvalidType invalidValue ->
            "Uh oh, I was expecting a string or object, but got \"" <> Text.pack (show invalidValue) <>
            "."

decodePrimitiveType :: BS.ByteString -> Either String Type
decodePrimitiveType = JsonBase.eitherDecode

-- RUNNER
parse :: String
parse = "hello, world"
