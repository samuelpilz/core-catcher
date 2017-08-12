module ClientState exposing (..)

import Protocol exposing (..)
import ProtocolUtils exposing (..)


type Msg
    = PlayerNameChange String
    | DoLogin Login
    | Clicked Node
    | SelectEnergy Energy
    | MsgFromServer MessageForClient
    | None


type ClientState
    = GameState_ GameState
    | PreGame_ PreGame


type alias PreGame =
    { server : String
    , playerNameField : String
    }


type alias GameState =
    { playerPositions : PlayerPositions
    , playerEnergies : PlayerEnergies
    , rogueHistory : RogueHistory -- TODO: possibility for open history
    , network : Network
    , player : Player
    , selectedEnergy : Energy
    , server : String
    , gameError : Maybe GameError
    , gameOver : Bool
    }


getServer : ClientState -> String
getServer state =
    case state of
        PreGame_ p ->
            p.server

        GameState_ g ->
            g.server


type alias Flags =
    { server : String }
