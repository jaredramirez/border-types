{-# LANGUAGE OverloadedStrings #-}

module Generate.Elm
  ( toModule
  , toCustomType
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

tab :: TypeString
tab = Types.TypeString "    "

noTab :: TypeString
noTab = Types.TypeString ""

toModule :: Text -> TypeString
toModule name = Types.TypeString $ "module " <> name <> " exposing (..)\n"

toCustomType :: CustomType -> TypeString
toCustomType customType =
  case customType of
    Types.Alias name value        -> aliasType name value
    Types.Union name constructors -> unionType name constructors

unionType :: Text -> HashMap Text [PrimitiveType] -> TypeString
unionType name subTypes =
  Types.TypeString ("type " <> name <> "\n") <>
  foldr
    (\cur acc -> tab <> cur <> acc)
    (Types.TypeString "")
    (case HashMap.toList subTypes of
       [] -> []
       head:tail -> unionTypeHelper '=' head : fmap (unionTypeHelper '|') tail)

unionTypeHelper :: Char -> (Text, [PrimitiveType]) -> TypeString
unionTypeHelper c (key, values) =
  Types.TypeString $
  Text.cons
    c
    (" " <> key <>
     (foldr (<>) "" .
      fmap
        (Types.getText .
         (\type' ->
            case type' of
              Types.Record subTypes ->
                Types.TypeString "\n" <>
                record (tab <> tab) (HashMap.map toPrimitiveType subTypes)
              _ -> Types.TypeString " " <> toPrimitiveType type')))
       values <>
     "\n")

aliasType :: Text -> PrimitiveType -> TypeString
aliasType name subType =
  (Types.TypeString $ "type alias " <> name <> " =\n") <>
  toPrimitiveTypeWithTab tab subType

toPrimitiveType :: PrimitiveType -> TypeString
toPrimitiveType = toPrimitiveTypeWithTab noTab

toPrimitiveTypeWithTab :: TypeString -> PrimitiveType -> TypeString
toPrimitiveTypeWithTab tab' type' =
  case type' of
    Types.String -> tab' <> string
    Types.Int -> tab' <> int
    Types.Float -> tab' <> float
    Types.Bool -> tab' <> bool
    Types.Unit -> tab' <> unit
    Types.List subType -> tab' <> list (toPrimitiveType subType)
    Types.Tuple subTypes -> tab' <> tuple (fmap toPrimitiveType subTypes)
    Types.Record subTypes -> record tab' (HashMap.map toPrimitiveType subTypes)

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
  "(" <> Text.intercalate ", " (fmap Types.getText subTypes) <> ")"

record :: TypeString -> HashMap Text TypeString -> TypeString
record tab subTypes =
  foldr (\cur acc -> tab <> cur <> acc) (Types.TypeString "") $
  case HashMap.toList subTypes of
    [] -> []
    head:tail ->
      recordHelper '{' head :
      fmap (recordHelper ',') tail ++ [Types.TypeString "}"]

recordHelper :: Char -> (Text, TypeString) -> TypeString
recordHelper c (key, value) =
  Types.TypeString $
  Text.cons c (" " <> key <> " : " <> Types.getText value <> "\n")
