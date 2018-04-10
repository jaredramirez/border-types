{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
  ( TypeString(..)
  , RootConfig(..)
  , LanguageConfig(..)
  , CustomType(..)
  , PrimitiveType(..)
  ) where

import qualified Control.Monad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Semigroup
import qualified Data.Text           as Text

newtype TypeString =
  TypeString Text.Text
  deriving (Eq, Show, Data.Semigroup.Semigroup)

data RootConfig = RootConfig
  { langauges :: [LanguageConfig]
  , types     :: [CustomType]
  } deriving (Show, Eq)

data LanguageConfig
  = ElmConfig { outputPath :: Text.Text }
  | ReasonConfig { outputPath :: Text.Text }
  deriving (Show, Eq)

data CustomType
  = Alias { name  :: Text.Text
          , value :: PrimitiveType }
  | Union { name         :: Text.Text
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
