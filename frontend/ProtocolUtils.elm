module ProtocolUtils exposing (..)

{-
   utility functions for dealing with protocol types
-}

import Protocol exposing (..)
import Maybe exposing (..)
import EveryDict exposing (EveryDict)


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


emptyNetwork : Network
emptyNetwork =
    { nodes = [], overlays = EveryDict.empty }

energyId : Energy -> Int
energyId e =
    case e of
        Red ->
            0

        Blue ->
            1

        Orange ->
            2
