module ClientState exposing (..)

import Protocol exposing (..)
import ProtocolUtils exposing (..)


type Msg
    = Clicked Node
    | SelectEnergy Transport
    | MsgFromServer MessageForClient
    | None


type alias ClientState =
    { gameView : GameView
    , network : Network
    , player : Player
    , selectedEnergy : Transport
    , server : String
    }


type alias Flags =
    { server : String }
