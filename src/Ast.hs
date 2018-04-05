module Ast
    ( CustomType(..)
    , PrimitiveType(..)
    ) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

newtype Config =
    Config [CustomType]

data CustomType
    = Alias { name :: Text.Text
            , value :: PrimitiveType }
    | Union { name :: Text.Text
            , constructors :: HashMap.HashMap Text.Text [PrimitiveType] }
    deriving (Show, Eq)

data PrimitiveType
    = String
    | Int
    | Float
    | Bool
    | Record (HashMap.HashMap Text.Text PrimitiveType)
    deriving (Show, Eq)
