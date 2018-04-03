module Main where

import qualified ParserSpec
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec $ do ParserSpec.spec
