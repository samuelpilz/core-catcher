module ClientState exposing (..)

import Protocol exposing (..)
import ProtocolUtils exposing (..)


type Msg
    = Clicked Node
    | SelectEnergy Energy
    | MsgFromServer MessageForClient
    | None


type alias ClientState =
    { gameView : GameView
    , network : Network
    , player : Player
    , selectedEnergy : Energy
    , server : String
    , gameError : Maybe GameError
    }


type alias Flags =
    { server : String }
