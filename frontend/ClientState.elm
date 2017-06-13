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
    , player : Player
    , selectedEnergy : Transport
    }

