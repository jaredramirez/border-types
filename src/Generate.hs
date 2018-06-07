{-# LANGUAGE OverloadedStrings #-}

module Generate where

import           Data.Semigroup  ((<>))
import           Data.Text       (Text)
import qualified Data.Text       as Text
import qualified Generate.Elm    as GenElm
import qualified Generate.Reason as GenReason
import           Types           (CustomType, LanguageConfig, TypeString)
import qualified Types

generate :: [CustomType] -> LanguageConfig -> TypeString
generate types langConfig =
  case langConfig of
    Types.ElmConfig outputPath ->
      let moduleName = getModuleName outputPath
      in GenElm.toModule moduleName <>
         foldr ((<>) . GenElm.toCustomType) (Types.TypeString "") types
    Types.ReasonConfig _ ->
      foldr ((<>) . GenReason.toCustomType) (Types.TypeString "") types

defaultModuleName :: Text
defaultModuleName = "Models"

getModuleName :: Text -> Text
getModuleName filePath =
  let (_, fileNameWithExt) = Text.breakOnEnd "/" filePath
      (fileName, _) = Text.breakOn "." fileNameWithExt
  in if Text.null fileName
       then defaultModuleName
       else fileName
