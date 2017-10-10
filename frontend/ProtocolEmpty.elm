module ProtocolEmpty exposing (..)

{-
   empty data for protocol types
-}

import Protocol exposing (..)
import Maybe exposing (..)
import EveryDict exposing (EveryDict)


emptyNetwork : Network
emptyNetwork =
    { nodes = [], overlays = EveryDict.empty }


emptyPlayer : Player
emptyPlayer =
    { playerName = "" }


emptyShadowRogueHistory : ShadowRogueHistory
emptyShadowRogueHistory =
    { shadowRogueHistory = [] }


emptyOpenRogueHistory : OpenRogueHistory
emptyOpenRogueHistory =
    { openRogueHistory = [] }


emptyPlayerPositions : PlayerPositions
emptyPlayerPositions =
    { playerPositions = EveryDict.empty }


emptyPlayerEnergies : PlayerEnergies
emptyPlayerEnergies =
    { playerEnergies = EveryDict.empty }


emptyNode : Node
emptyNode =
    { nodeId = 0 }


emptyRogueView : RogueGameView
emptyRogueView =
    { roguePlayerPositions = emptyPlayerPositions
    , rogueEnergies = emptyPlayerEnergies
    , rogueOwnHistory = emptyShadowRogueHistory
    , rogueNextPlayer = emptyPlayer
    }


emptyGameView : GameView
emptyGameView =
    RogueView emptyRogueView


emptyInitialInfo : InitialInfoGameActive
emptyInitialInfo =
    { networkForGame = emptyNetwork
    , initialGameView = emptyGameView
    , startingPlayer = emptyPlayer
    , initialInfoAllPlayers = []
    , initialInfoAllEnergies = []
    , initialInfoGameName = ""
    }


emptyError : GameError
emptyError =
    GameIsOver


emptyGameOverView : GameOverView
emptyGameOverView =
    { gameOverViewPlayerPositions = emptyPlayerPositions
    , gameOverViewPlayerEnergies = emptyPlayerEnergies
    , gameOverViewRogueHistory = emptyOpenRogueHistory
    , gameOverViewWinningPlayer = emptyPlayer
    , gameOverViewNetwork = emptyNetwork
    , gameOverViewAllPlayers = []
    , gameOverViewAllEnergies = []
    , gameOverViewGameName = ""
    }


emptyLogin : Login
emptyLogin =
    { loginPlayer = emptyPlayer }


emptyLoginFail : LoginFail
emptyLoginFail =
    { loginFailPlayer = emptyPlayer }


emptyPlayerHome : PlayerHome
emptyPlayerHome =
    { playerHomePlayer = emptyPlayer
    , activeGames = []
    , activeLobbies = []
    }


emptyGameLobbyView : GameLobbyView
emptyGameLobbyView =
    { gameLobbyViewGameName = ""
    , gameLobbyViewPlayers = []
    }


emptyCreateNewGame : CreateNewGame
emptyCreateNewGame =
    { createGameName = ""
    }


emptyGameId : GameId
emptyGameId =
    { gameId = 0 }


emptyJoinGame : JoinGame
emptyJoinGame =
    { joinGameId = emptyGameId }
