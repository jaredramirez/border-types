{-# LANGUAGE OverloadedStrings #-}

module Generate.Reason
  ( toCustomType
  , toPrimitiveType
  ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import           Data.Semigroup      ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Misc
import           Types               (CustomType, PrimitiveType, TypeString)
import qualified Types

tab :: Text
tab = "  "

newLine :: TypeString
newLine = Types.TypeString "\n"

toCustomType :: CustomType -> TypeString
toCustomType customType =
  case customType of
    Types.Alias name value        -> aliasType name value
    Types.Union name constructors -> unionType name constructors

unionType :: Text -> HashMap Text [PrimitiveType] -> TypeString
unionType name subTypes =
  let constructors = HashMap.toList subTypes
  in (Types.TypeString $
      "type " <> Text.toLower name <> " =\n" <>
      (flip Text.snoc ';' .
       Text.concat . List.intersperse "\n" . fmap toConstructors)
        constructors) <>
     newLine

toConstructors :: (Text, [PrimitiveType]) -> Text
toConstructors (name, types) =
  tab <> "| " <> name <>
  (if List.null types
     then ""
     else "(" <>
          Text.intercalate
            ", "
            (fmap
               (Types.getText .
                toPrimitiveTypeWithRecordTab (Types.TypeString tab))
               types) <>
          ")")

aliasType :: Text -> PrimitiveType -> TypeString
aliasType name subType =
  (Types.TypeString $ "type " <> Text.toLower name <> " = ") <>
  toPrimitiveType subType <>
  Types.TypeString ";" <>
  newLine <>
  newLine

toPrimitiveType :: PrimitiveType -> TypeString
toPrimitiveType = toPrimitiveTypeWithRecordTab (Types.TypeString "")

toPrimitiveTypeWithRecordTab :: TypeString -> PrimitiveType -> TypeString
toPrimitiveTypeWithRecordTab tab' type' =
  case type' of
    Types.String -> string
    Types.Int -> int
    Types.Float -> float
    Types.Bool -> bool
    Types.Unit -> unit
    Types.List subType -> list (toPrimitiveType subType)
    Types.Tuple subTypes -> tuple (fmap toPrimitiveType subTypes)
    Types.Record subTypes -> record tab' (HashMap.map toPrimitiveType subTypes)

string :: TypeString
string = Types.TypeString "string"

int :: TypeString
int = Types.TypeString "int"

float :: TypeString
float = Types.TypeString "float"

bool :: TypeString
bool = Types.TypeString "bool"

unit :: TypeString
unit = Types.TypeString "unit"

list :: TypeString -> TypeString
list subType = Types.TypeString "list(" <> subType <> Types.TypeString ")"

tuple :: [TypeString] -> TypeString
tuple subTypes =
  Types.TypeString $
  "(" <> Text.intercalate ", " (fmap Types.getText subTypes) <> ")"

record :: TypeString -> HashMap Text TypeString -> TypeString
record tab' subTypes =
  let tabText = Types.getText tab'
  in Types.TypeString $
     "{\n" <> tab <> tabText <> ".\n" <>
     Text.concat
       (fmap
          (\(key, value) ->
             tab <> tabText <> "\"" <> key <> "\": " <> Types.getText value <>
             ",\n")
          (HashMap.toList subTypes)) <>
     tabText <>
     "}"
