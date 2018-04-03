module Main where

import qualified Parser as P

main :: IO ()
main = do
    putStrLn P.parse
