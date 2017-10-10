module Experimental.ClientState exposing (..)

import Protocol exposing (..)
import Time exposing (..)
import AllDict exposing (..)
import EveryDict exposing (..)
import View.GameViewDisplay exposing (GameViewDisplayInfo)


-- TODO document messages
-- TODO: split messages for scoping


type Msg
    = PlayerNameChange String
    | DoLogin
    | DoLogout
    | GameAction_ GameAction
    | SelectEnergy Energy
    | MsgFromServer MessageForClient
    | Tick Time
    | DoPlayerHomeRefresh
    | DoOpenNewGame
    | DoCreateGame
    | DoJoinGame GameId
    | DoStartGame
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
    | LoggedOut


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
    , playerHomeContent : PlayerHomeContent
    }


type alias PlayerHomeContent =
    { activeLobbies : Maybe (List GameLobbyPreview)
    , activeGames : Maybe (List GamePreview)
    }



-- |Model for logged in player.
-- TODO: remove disconnect from state here...


type LoggedInState
    = InPlayerHome
    | JoinGamePending
    | NewGame
    | NewGamePending
    | GameConnectPending
    | GameOver_ GameOver



-- |Model for in-game. Contains state of the game and information to display the network.


type alias GameOver =
    { networkModel : NetworkModel }


type alias InGame =
    { player : Player
    , gameName : String

    --    , networkViewModel : NetworkModel
    }


type InGameState
    = InPreGameLobby_ InPreGameLobby
    | GameActive_ GameActiveState GameActive
    | GameDisconnected
    | GameServerReconnected


type alias InPreGameLobby =
    { players : List Player
    }


type GameActiveState
    = YourTurn
    | ActionPending
    | OthersTurn


type alias GameActive =
    { networkModel : NetworkModel
    }


type alias PlayerMovementAnimation =
    { fromNode : Node
    , toNode : Node
    , startTime : Time
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
