{-# LANGUAGE OverloadedStrings #-}

module Generate.Reason where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Semigroup      ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Misc
import           Types               (PrimitiveType, TypeString)
import qualified Types

tab :: Text
tab = "  "

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
  "(" <> Text.intercalate ", " (fmap Misc.extract subTypes) <> ")"

record :: HashMap Text TypeString -> TypeString
record subTypes =
  Types.TypeString $
  "{.\n" <>
  Text.intercalate
    ",\n"
    (fmap
       (\(key, value) -> tab <> key <> ": " <> Misc.extract value)
       (HashMap.toList subTypes)) <>
  "}"
