module Parser.Misc
  ( jsonValueToText
  ) where

import qualified Data.Aeson.BetterErrors as Json
import qualified Data.Aeson.Types        as JsonTypes
import qualified Data.Text               as Text

jsonValueToText :: JsonTypes.Value -> Text.Text
jsonValueToText = Text.drop 2 . Text.pack . show . Json.jsonTypeOf
