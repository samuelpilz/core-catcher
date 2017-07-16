module ProtocolUtils exposing (..)

{-
   utility functions for dealing with protocol types
-}

import Protocol exposing (..)
import List exposing (..)
import Tuple as Tuple
import Maybe exposing (..)
import EveryDict exposing (EveryDict)


playerPositions : GameView -> PlayerPositions
playerPositions gameView =
    case gameView of
        RogueView view ->
            view.roguePlayerPositions

        CatcherView view ->
            view.catcherPlayerPositions


energies : GameView -> PlayerEnergies
energies gameView =
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


getEnergyForTransportAndPlayer : Player -> Transport -> GameView -> Int
getEnergyForTransportAndPlayer player transport gameView =
    EveryDict.get player (energies gameView).playerEnergies
        |> Maybe.andThen (\energies -> EveryDict.get transport energies.energyMap)
        |> Maybe.withDefault 0
