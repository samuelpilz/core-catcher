module ClientState exposing (..)

import Protocol exposing (..)
import ProtocolUtils exposing (..)


type Msg
    = Clicked Node
    | SelectEnergy Energy
    | MsgFromServer MessageForClient
    | None


type alias ClientState =
    { playerPositions : PlayerPositions
    , playerEnergies : PlayerEnergies
    , rogueHistory : RogueHistory -- TODO: possibility for open history
    --, gameView : GameView
    , network : Network
    , player : Player
    , selectedEnergy : Energy
    , server : String
    , gameError : Maybe GameError
    , gameOver : Bool
    }


type alias Flags =
    { server : String }
