module View.PlayerAnimation exposing (positionInSvg)

import Protocol exposing (..)
import ProtocolUtils exposing (..)
import View.GameViewDisplay exposing (..)
import Time exposing (Time)
import AllDict exposing (..)
import EveryDict exposing (..)
import ClientState exposing (..)
import Animation exposing (..)
import Ease exposing (..)


-- |exposed function: examine xy coordinates of one player for given game-state and display-info


positionInSvg : GameViewDisplayInfo -> GameState -> Player -> ( Int, Int )
positionInSvg displayInfo gameState player =
    case AllDict.get player <| framePositionsFromState gameState of
        Just (AtNode node) ->
            ( nodeX displayInfo.nodeXyMap node, nodeY displayInfo.nodeXyMap node )

        Just (InMovement playerMovement) ->
            animationPositionInSvg displayInfo gameState playerMovement

        Nothing ->
            ( -50, -50 )


type PlayerPositionInFrame
    = AtNode Node
    | InMovement PlayerMovementAnimation


type alias PlayerPositionsInFrame =
    AllDict Player PlayerPositionInFrame String


framePositionsFromState : GameState -> PlayerPositionsInFrame
framePositionsFromState state =
    AllDict.fromList .playerName <|
        (List.map (\( p, n ) -> ( p, AtNode n ))
            -- filter players with active animations
            << List.filter (\( p, _ ) -> Nothing == AllDict.get p state.activeAnimations)
            << EveryDict.toList
         <|
            state.playerPositions.playerPositions
        )
            ++ (List.map (\( p, m ) -> ( p, InMovement m ))
                    << AllDict.toList
                <|
                    state.activeAnimations
               )


animationPositionInSvg : GameViewDisplayInfo -> GameState -> PlayerMovementAnimation -> ( Int, Int )
animationPositionInSvg displayInfo gameState playerMovement =
    let
        moveRatio =
            animate gameState.animationTime <|
                animationFunction playerMovement.startTime displayInfo.movementAnimationDuration

        posX =
            (moveRatio
                * (toFloat <| nodeX displayInfo.nodeXyMap playerMovement.toNode)
                + (1 - moveRatio)
                * (toFloat <| nodeX displayInfo.nodeXyMap playerMovement.fromNode)
            )

        posY =
            (moveRatio
                * (toFloat <| nodeY displayInfo.nodeXyMap playerMovement.toNode)
                + (1 - moveRatio)
                * (toFloat <| nodeY displayInfo.nodeXyMap playerMovement.fromNode)
            )
    in
        ( round <| posX, round <| posY )


animationFunction : Time -> Time -> Animation
animationFunction startTime animationDuration =
    duration animationDuration
        << ease (bezier 0.25 0.1 0.25 1)
        << animation
    <|
        startTime
