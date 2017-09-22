module Experimental.ClientState exposing (..)

import Protocol exposing (..)
import ProtocolUtils exposing (..)
import Time exposing (..)
import AllDict exposing (..)
import EveryDict exposing (..)
import View.GameViewDisplay exposing (GameViewDisplayInfo)
import Example.ExampleGameViewDisplay as Example


-- TODO document
-- TODO: split messages for scoping

type Msg
    = PlayerNameChange String
    | DoLogin
    | GameAction_ GameAction
    | SelectEnergy Energy
    | MsgFromServer MessageForClient
    | Tick Time
    | DoOpenNewGame
    | DoCreateGame
    | DoStartGame
    | DoJoinGame
    | DoGameReconnect
    | DoGameConnect
    | ToHome
    | ConnectionLost
    | None

type GameAction = Movement Node


{-
   complex states:
   * global state
   * InGame
   * GameActive
   * LoggedIn

-}
-- |Model for this elm-app. This contains all possible states the game can be in.


type alias ClientModel =
    { state : ClientState
    , server : String
    }


type ClientState
    = Landing_ Landing
    | LandingConnected_ LandingConnected
    | LoginPending_ LoginPending
    | LoginFailed_
    | LoggedIn_ LoggedInState LoggedIn
    | InGame_ InGameState InGame


type alias Landing =
    { playerNameField : String
    }

type alias LandingConnected =
    { playerNameField : String
    }


type alias LoginPending =
    { player : Player
    }


type alias LoggedIn =
    { player : Player
    }



-- |Model for logged in player.


type LoggedInState
    = PlayerHome
    | Disconnected
    | ServerReconnected
    | JoinGamePending
    | NewGame
    | NewGamePending
    | PreGameLobby -- TODO: rename due to name clash with protocol
    | GameStartPending
    | GameConnectPending



-- |Model for in-game. Contains state of the game and information to display the network.


type alias InGame =
    { player : Player
--    , networkViewModel : NetworkModel
    }



-- |Type containing the information necessary to display the network.


type alias NetworkModel =
    { network : Network
    , player : Player
    , players : List Player
    , energies : List Energy
    , playerPositions : PlayerPositions
    , playerEnergies : PlayerEnergies
    , rogueHistory : RogueHistory
    , nextPlayer : Maybe Player
    , selectedEnergy : Maybe Energy
    , gameError : Maybe GameError
    , gameOver : Bool
    , animationTime : Time
    , activeAnimations : AllDict Player PlayerMovementAnimation String
    , displayInfo : GameViewDisplayInfo
    }


type InGameState
    = GameActive_ GameActive
    | GameDisconnected
    | GameServerReconnected
    | GameReconnectPending
    | GameOver


type GameActive
    = YourTurn
    | ActionPending
    | OthersTurn



type alias PlayerMovementAnimation =
    { fromNode : Node
    , toNode : Node
    , startTime : Time
    }
