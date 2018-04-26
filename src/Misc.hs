module Misc where

import qualified Control.Arrow as Arrow
import qualified Data.Text     as Text

applyTuple :: a -> (a -> b, a -> b') -> (b, b')
applyTuple a (f, f') = (f a, f' a)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, a') = (f a, f a')

apply :: a -> (a -> b) -> b
apply a f = f a

-- Inflix fmap flipped, not added to base until 4.11
-- Really helpful for chaining fmap & bind
(<&>) :: Functor f => f a -> (a -> b) -> f b
a <&> f = f <$> a

infixl 1 <&>

-- This is just an alias to make the how I'm using the function more clear
-- This is a really cool usage of how (->) is an ADT in haskell, checkout
-- https://stackoverflow.com/a/13504032/8838731!
mapLeft :: (b -> c) -> Either b a -> Either c a
mapLeft = Arrow.left
