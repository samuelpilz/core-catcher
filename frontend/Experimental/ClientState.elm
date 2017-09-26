module Experimental.ClientState exposing (..)

import Protocol exposing (..)
import ProtocolUtils exposing (..)
import Time exposing (..)
import AllDict exposing (..)
import EveryDict exposing (..)
import View.GameViewDisplay exposing (GameViewDisplayInfo)
import Example.ExampleGameViewDisplay as Example


-- TODO document messages
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
    | DoJoinGame
    | DoGameConnect
    | ToHome
    | ToLandingPage
    | ConnectionLost
    | None


type GameAction
    = Movement Node



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
    = LandingArea_ LandingAreaState LandingArea
    | LoggedIn_ LoggedInState LoggedIn
    | InGame_ InGameState InGame
    | Disconnected DisconnectReconnect
    | Reconnected DisconnectReconnect


type LandingAreaState
    = Landing
    | LandingConnected
    | LoginPending_ LoginPending
    | LoginFailed


type alias LandingArea =
    { playerNameField : String
    }


type alias LoginPending =
    { pendingPlayer : Player
    }


-- TODO: think about what needs to be stored in disconnect/reconnect
-- TODO: think about name
-- TODO: think about to what is returned after a lost connection
type alias DisconnectReconnect =
    { loggedIn : LoggedIn
    }


type alias LoggedIn =
    { player : Player
    }



-- |Model for logged in player.
-- TODO: remove disconnect from state here...


type LoggedInState
    = PlayerHome
    | JoinGamePending
    | NewGame
    | NewGamePending
    | PreGameLobby
    | GameConnectPending
    | GameOver



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


type GameActive
    = YourTurn
    | ActionPending
    | OthersTurn


type alias PlayerMovementAnimation =
    { fromNode : Node
    , toNode : Node
    , startTime : Time
    }
