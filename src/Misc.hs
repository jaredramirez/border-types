module Misc where

import qualified Control.Arrow as Arrow
import qualified Data.Text     as Text
import qualified Types

apply :: a -> (a -> b) -> b
apply a f = f a

-- This is just an alias to make the how I'm using the function more clear
-- This is a really cool usage of how (->) is an ADT in haskell, checkout
-- https://stackoverflow.com/a/13504032/8838731!
mapLeft :: (b -> c) -> Either b a -> Either c a
mapLeft = Arrow.left

extract :: Types.TypeString -> Text.Text
extract (Types.TypeString value) = value
