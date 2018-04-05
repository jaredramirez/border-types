module Misc where

import qualified Control.Arrow as Arrow

apply :: a -> (a -> b) -> b
apply a f = f a

-- Another alias because I like (|>) better than (&)
(|>) :: a -> (a -> b) -> b
x |> f = apply x f

infixl 0 |>

-- This is just an alias to make it more clear how it's used
mapLeft :: (Arrow.ArrowChoice a) => a b c -> a (Either b d) (Either c d)
mapLeft = Arrow.left
