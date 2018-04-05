module Main where

import qualified AstSpec.ParseSpec
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec $ do AstSpec.ParseSpec.spec
