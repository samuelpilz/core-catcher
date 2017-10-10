module View.PlayerAnimation exposing (positionInSvg, updateActiveAnimations)

-- TODO: remove this from View-module

import Protocol exposing (..)
import ProtocolUtils exposing (..)
import View.GameViewDisplay exposing (..)
import Time exposing (Time)
import AllDict exposing (..)
import EveryDict exposing (..)
import Animation exposing (..)
import Ease exposing (..)
import Experimental.ClientState exposing (..)


-- |exposed function: examine xy coordinates of one player for given game-state and display-info


positionInSvg : NetworkModel -> Player -> ( Int, Int )
positionInSvg networkModel player =
    let
        displayInfo =
            networkModel.displayInfo
    in
        case AllDict.get player <| framePositionsFromState networkModel of
            Just (AtNode node) ->
                ( nodeX displayInfo.nodeXyMap node, nodeY displayInfo.nodeXyMap node )

            Just (InMovement playerMovement) ->
                animationPositionInSvg networkModel playerMovement

            Nothing ->
                ( -50, -50 )


type PlayerPositionInFrame
    = AtNode Node
    | InMovement PlayerMovementAnimation


type alias PlayerPositionsInFrame =
    AllDict Player PlayerPositionInFrame String


framePositionsFromState : NetworkModel -> PlayerPositionsInFrame
framePositionsFromState networkModel =
    AllDict.fromList .playerName <|
        (List.map (\( p, n ) -> ( p, AtNode n ))
            -- filter players with active animations
            << List.filter (\( p, _ ) -> Nothing == AllDict.get p networkModel.activeAnimations)
            << EveryDict.toList
         <|
            networkModel.playerPositions.playerPositions
        )
            ++ (List.map (\( p, m ) -> ( p, InMovement m ))
                    << AllDict.toList
                <|
                    networkModel.activeAnimations
               )


animationPositionInSvg : NetworkModel -> PlayerMovementAnimation -> ( Int, Int )
animationPositionInSvg networkModel playerMovement =
    let
        displayInfo =
            networkModel.displayInfo

        moveRatio =
            animate networkModel.animationTime <|
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


-- |Computes the all active animations with the given player-position map
updateActiveAnimations : NetworkModel -> PlayerPositions -> AllDict Player PlayerMovementAnimation String
updateActiveAnimations networkModel playerPositions =
    let
        movingPlayer =
            networkModel.nextPlayer

        newAnimation =
            Maybe.map2
                (\fromNode toNode ->
                    { fromNode = fromNode
                    , toNode = toNode
                    , startTime = networkModel.animationTime
                    }
                )
                -- fromNode
                (Maybe.andThen
                    (\p ->
                        EveryDict.get p
                            networkModel.playerPositions.playerPositions
                    )
                    movingPlayer
                )
                -- toNode
                (Maybe.andThen
                    (\p -> EveryDict.get p playerPositions.playerPositions)
                    movingPlayer
                )
    in
        --        log "active animations" <|
        case ( newAnimation, movingPlayer ) of
            ( Just a, Just p ) ->
                AllDict.insert p a networkModel.activeAnimations

            _ ->
                networkModel.activeAnimations
