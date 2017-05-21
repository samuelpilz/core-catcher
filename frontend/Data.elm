module Data exposing (..)


type alias Model =
    ( Int, Result String Int )


type Msg
    = Receive String
    | Send
    | UpdateNum String
    | Clicked Int

