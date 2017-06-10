module ClientState exposing (..)

import Protocol exposing (..)
import ProtocolUtils exposing (..)

type Msg
    = Clicked Node
    | SelectEnergy Transport
    | Received String

type alias ClientState =
    { gameView : GameView
    , selectedEnergy : Transport
    }

