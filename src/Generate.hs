{-# LANGUAGE OverloadedStrings #-}

module Generate where

import           Data.Semigroup  ((<>))
import qualified Generate.Elm    as GenElm
import qualified Generate.Reason as GenReason
import           Types           (CustomType, LanguageConfig, TypeString)
import qualified Types

generate :: [CustomType] -> LanguageConfig -> TypeString
generate types langConfig =
  case langConfig of
    Types.ElmConfig _ ->
      GenElm.toModule "Models" <>
      foldr ((<>) . GenElm.toCustomType) (Types.TypeString "") types
    Types.ReasonConfig _ ->
      foldr ((<>) . GenReason.toCustomType) (Types.TypeString "") types
