module Data exposing (Msg(..))

import Protocol exposing (..)

type Msg
    = Clicked Node
    | Received String 
