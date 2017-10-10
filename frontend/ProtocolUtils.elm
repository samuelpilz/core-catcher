module ProtocolUtils exposing (..)

{-
   utility functions for dealing with protocol types
-}

import Protocol exposing (..)
import Maybe exposing (..)
import EveryDict exposing (EveryDict)
import Experimental.ClientState exposing (..)
import AllDict
import View.GameViewDisplay exposing (..)


playerPositions : GameView -> PlayerPositions
playerPositions gameView =
    case gameView of
        RogueView view ->
            view.roguePlayerPositions

        CatcherView view ->
            view.catcherPlayerPositions


playerEnergies : GameView -> PlayerEnergies
playerEnergies gameView =
    case gameView of
        RogueView view ->
            view.rogueEnergies

        CatcherView view ->
            view.catcherEnergies


rogueHistory : GameView -> RogueHistory
rogueHistory gameView =
    ShadowHistory <|
        case gameView of
            RogueView view ->
                view.rogueOwnHistory

            CatcherView view ->
                view.catcherRogueHistory


nextPlayer : GameView -> Player
nextPlayer gameView =
    case gameView of
        RogueView view ->
            view.rogueNextPlayer

        CatcherView view ->
            view.catcherNextPlayer


getEnergyForEnergyAndPlayer : Player -> Energy -> PlayerEnergies -> Int
getEnergyForEnergyAndPlayer player energy playerEnergies =
    Maybe.withDefault 0
        << Maybe.andThen (EveryDict.get energy)
        << Maybe.map .energyMap
        << EveryDict.get player
    <|
        playerEnergies.playerEnergies


energyId : Energy -> Int
energyId e =
    case e of
        Red ->
            0

        Blue ->
            1

        Orange ->
            2



-- TODO: move to own module for state-transformations / utils


networkModelFromInitInfo : GameViewDisplayInfo -> Player -> InitialInfoGameActive -> NetworkModel
networkModelFromInitInfo displayInfo player initInfo =
    { network = initInfo.networkForGame
    , player = player
    , players = initInfo.initialInfoAllPlayers
    , energies = initInfo.initialInfoAllEnergies
    , playerPositions = playerPositions initInfo.initialGameView
    , playerEnergies = playerEnergies initInfo.initialGameView
    , rogueHistory = rogueHistory initInfo.initialGameView
    , nextPlayer = Just initInfo.startingPlayer
    , selectedEnergy = Nothing
    , gameError = Nothing
    , gameOver = False
    , animationTime = 0
    , activeAnimations = AllDict.empty .playerName
    , displayInfo = displayInfo
    }


networkModelFromGameOverView : GameViewDisplayInfo -> Player -> GameOverView -> NetworkModel
networkModelFromGameOverView displayInfo player gameOverView =
    { network = gameOverView.gameOverViewNetwork
    , player = player
    , players = gameOverView.gameOverViewAllPlayers
    , energies = gameOverView.gameOverViewAllEnergies
    , playerPositions = gameOverView.gameOverViewPlayerPositions
    , playerEnergies = gameOverView.gameOverViewPlayerEnergies
    , rogueHistory = OpenHistory gameOverView.gameOverViewRogueHistory
    , nextPlayer = Nothing
    , selectedEnergy = Nothing
    , gameError = Nothing
    , gameOver = False
    , animationTime = 0
    , activeAnimations = AllDict.empty .playerName
    , displayInfo = displayInfo
    }
