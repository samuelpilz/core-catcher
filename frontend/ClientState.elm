module ClientState exposing (..)

import Protocol exposing (..)
import ProtocolUtils exposing (..)
import Time exposing (..)
import AllDict exposing (..)
import EveryDict exposing (..)
import View.GameViewDisplay exposing (GameViewDisplayInfo)
import Example.ExampleGameViewDisplay as Example
import ProtocolEmpty exposing (..)


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
    , rogueHistory : RogueHistory
    , nextPlayer : Maybe Player
    , player : Player
    , selectedEnergy : Maybe Energy
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


emptyGameState : String -> Player -> GameState
emptyGameState server player =
    { network = ProtocolEmpty.emptyNetwork
    , players = []
    , energies = []
    , playerPositions = { playerPositions = EveryDict.empty }
    , playerEnergies = { playerEnergies = EveryDict.empty }
    , rogueHistory = ShadowHistory { shadowRogueHistory = [] }
    , nextPlayer = Nothing
    , selectedEnergy = Nothing
    , gameError = Nothing
    , gameOver = False
    , server = server
    , player = player
    , animationTime = 0
    , activeAnimations = AllDict.empty .playerName
    , displayInfo = Example.displayInfo
    }


emptyPreGame : String -> PreGame
emptyPreGame server =
    { server = server
    , playerNameField = ""
    }
