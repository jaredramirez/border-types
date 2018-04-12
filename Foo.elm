module Main exposing (..)


type Status
    = Okay
    | Bad
        { world : Int
        , hello : String
        }
    | Good String
