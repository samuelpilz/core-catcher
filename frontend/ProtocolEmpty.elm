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


emptyInitialInfo : InitialInfoForGame
emptyInitialInfo =
    { networkForGame = emptyNetwork
    , initialGameView = emptyGameView
    , initialPlayer = emptyPlayer
    , allPlayers = []
    , allEnergies = []
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
    }


emptyLoginSuccess : LoginSuccess
emptyLoginSuccess =
    { loginSuccessPlayer = emptyPlayer }


emptyLoginFail : LoginFail
emptyLoginFail =
    { loginFailPlayer = emptyPlayer }
