module ClientState exposing (..)

import Protocol exposing (..)
import ProtocolUtils exposing (..)
import Time exposing (..)
import AllDict exposing (..)
import View.GameViewDisplay exposing (GameViewDisplayInfo)


type Msg
    = PlayerNameChange String
    | DoLogin Login
    | Movement Node
    | SelectEnergy Energy
    | MsgFromServer MessageForClient
    | Tick Time
    | None


type ClientState
    = GameState_ GameState
    | PreGame_ PreGame


type alias PreGame =
    { server : String
    , playerNameField : String
    }


type alias GameState =
    { network : Network
    , players : List Player
    , energies : List Energy
    , playerPositions : PlayerPositions
    , playerEnergies : PlayerEnergies
    , rogueHistory : RogueHistory -- TODO: possibility for open history
    , nextPlayer : Maybe Player
    , player : Player
    , selectedEnergy : Energy
    , server : String
    , gameError : Maybe GameError
    , gameOver : Bool
    , displayInfo : GameViewDisplayInfo
    , animationTime : Time
    , activeAnimations : AllDict Player PlayerMovementAnimation String
    }


getServer : ClientState -> String
getServer state =
    case state of
        PreGame_ p ->
            p.server

        GameState_ g ->
            g.server


type alias PlayerMovementAnimation =
    { fromNode : Node
    , toNode : Node
    , startTime : Time
    }
