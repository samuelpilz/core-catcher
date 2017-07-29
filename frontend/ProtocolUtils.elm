module ProtocolUtils exposing (..)

{-
   utility functions for dealing with protocol types
-}

import Protocol exposing (..)
import List exposing (..)
import Tuple as Tuple
import Maybe exposing (..)


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


getFromList : k -> List ( k, v ) -> Maybe v
getFromList k list =
    List.head
        << List.map Tuple.second
        << List.filter ((==) k << Tuple.first)
    <|
        list


getEnergyForEnergyAndPlayer : Player -> Energy -> PlayerEnergies -> Int
getEnergyForEnergyAndPlayer player energy playerEnergies =
    Maybe.withDefault 0
        << Maybe.andThen (getFromList energy)
        << Maybe.map .energyMap
        << getFromList player
    <|
        playerEnergies.playerEnergies


emptyNetwork : Network
emptyNetwork =
    { nodes = [], overlays = [] }


emptyRogueView : RogueGameView
emptyRogueView =
    { roguePlayerPositions = { playerPositions = [] }
    , rogueEnergies = { playerEnergies = [] }
    , rogueOwnHistory = { rogueHistory = [] }
    , rogueNextPlayer = { playerId = 0 }
    }


energyId : Energy -> Int
energyId e =
    case e of
        Red ->
            0

        Blue ->
            1

        Orange ->
            2
