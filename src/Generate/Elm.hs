{-# LANGUAGE OverloadedStrings #-}

module Generate.Elm
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
tab = "    "

toCustomType :: CustomType -> TypeString
toCustomType customType =
  case customType of
    Types.Alias name value        -> aliasType name value
    Types.Union name constructors -> unionType name constructors

unionType :: Text -> HashMap Text [PrimitiveType] -> TypeString
unionType name subTypes =
  let constructors = HashMap.toList subTypes
  in Types.TypeString $
     "type " <> name <> "\n" <>
     (Text.append (tab <> "=") .
      Text.drop 5 . Text.concat . List.intersperse "\n" . fmap toConstructors)
       constructors

toConstructors :: (Text, [PrimitiveType]) -> Text
toConstructors (name, types) =
  tab <> "| " <> name <>
  (if List.null types
     then ""
     else " " <>
          Text.intercalate " " (fmap (Misc.extract . toPrimitiveType) types))

aliasType :: Text -> PrimitiveType -> TypeString
aliasType name subType =
  let subTypeString = toPrimitiveType subType
  in (Types.TypeString $ "type alias " <> name <> " =") <>
     case subType of
       Types.Record _ -> Types.TypeString "\n" <> indent subTypeString
       _              -> Types.TypeString " " <> subTypeString

indent :: TypeString -> TypeString
indent =
  Types.TypeString .
  Text.dropEnd 1 .
  Text.unlines . fmap (Text.append tab) . Text.lines . Misc.extract

toPrimitiveType :: PrimitiveType -> TypeString
toPrimitiveType type' =
  case type' of
    Types.String          -> string
    Types.Int             -> int
    Types.Float           -> float
    Types.Bool            -> bool
    Types.Unit            -> unit
    Types.List subType    -> list (toPrimitiveType subType)
    Types.Tuple subTypes  -> tuple (fmap toPrimitiveType subTypes)
    Types.Record subTypes -> record (HashMap.map toPrimitiveType subTypes)

string :: TypeString
string = Types.TypeString "String"

int :: TypeString
int = Types.TypeString "Int"

float :: TypeString
float = Types.TypeString "Float"

bool :: TypeString
bool = Types.TypeString "Bool"

unit :: TypeString
unit = Types.TypeString "()"

list :: TypeString -> TypeString
list subType = Types.TypeString "(List " <> subType <> Types.TypeString ")"

tuple :: [TypeString] -> TypeString
tuple subTypes =
  Types.TypeString $
  "(" <> Text.intercalate ", " (fmap Misc.extract subTypes) <> ")"

record :: HashMap Text TypeString -> TypeString
record subTypes =
  Types.TypeString $
  "{ " <>
  Text.intercalate
    ", "
    (fmap
       (\(key, value) -> key <> " : " <> Misc.extract value <> "\n")
       (HashMap.toList subTypes)) <>
  "}"
