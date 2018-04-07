module Ast
    ( Config(..)
    , LanguageConfig(..)
    , CustomType(..)
    , PrimitiveType(..)
    ) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

data Config = Config
    { langauges :: [LanguageConfig]
    , types :: [CustomType]
    } deriving (Show, Eq)

data LanguageConfig
    = ElmConfig { outputPath :: Text.Text }
    | ReasonConfig { outputPath :: Text.Text }
    deriving (Show, Eq)

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
    | Unit
    | List PrimitiveType
    | Tuple [PrimitiveType]
    | Record (HashMap.HashMap Text.Text PrimitiveType)
    deriving (Show, Eq)
