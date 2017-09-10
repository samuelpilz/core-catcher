module View.MapView exposing (mapView)

import Html as Html
import Html.Attributes as Html
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)
import EveryDict exposing (..)
import AllDict exposing (..)
import ClientState exposing (..)
import Protocol exposing (..)
import ProtocolUtils exposing (..)
import View.GameViewDisplay exposing (..)
import Debug exposing (log)
import View.PlayerAnimation exposing (..)


{-
   TODO: how to implement animated player positions?

   draw all playerPositions not possible.

   new datatype for drawing players:
   type PlayerPositionInFrame = AtNode Node | InMovement PlayerMovementAnimation
   playerCircle takes PlayerPositionsInFrame together with rest...

-}


mapView : Network -> GameViewDisplayInfo -> GameState -> Html.Html Msg
mapView network displayInfo gameState =
    svg
        [ height << toString <| displayInfo.mapHeight
        , width << toString <| displayInfo.mapWidth
        ]
    -- elements of svg now
    <|
        []
            ++ List.concatMap
                -- overlays
                (mapViewOfNetworkOverlayName displayInfo network)
                (List.sortBy (\( transport, _ ) -> getPriority displayInfo transport) <| EveryDict.toList network.overlays)
            -- base network
            ++ List.map (nodeCircle displayInfo.nodeXyMap) network.nodes
            ++ List.map (playerCircle displayInfo gameState)
                (EveryDict.toList gameState.playerPositions.playerPositions)
            ++ gameErrorText gameState.gameError
            ++ gameOverText gameState.gameOver


mapViewOfNetworkOverlayName :
    GameViewDisplayInfo
    -> Network
    -> ( Energy, NetworkOverlay )
    -> List (Svg.Svg Msg)
mapViewOfNetworkOverlayName displayInfo { overlays } ( overlayName, overlay ) =
    (Maybe.withDefault []
        << Maybe.map2 (mapViewOfNetworkOverlay displayInfo.nodeXyMap)
            (displayInfoForEnergy displayInfo overlayName)
     <|
        Just overlay
    )


mapViewOfNetworkOverlay : NodeXyMap -> OverlayDisplayInfo -> NetworkOverlay -> List (Svg Msg)
mapViewOfNetworkOverlay nodeXyMap { color, edgeWidth, nodeSize } { overlayNodes, overlayEdges } =
    List.map (edgeLine nodeXyMap color edgeWidth) overlayEdges
        ++ List.map (nodeCircleStop nodeXyMap color nodeSize) overlayNodes



-- TODO: fix xymap
-- svg create functions


gameErrorText : Maybe GameError -> List (Svg Msg)
gameErrorText errMay =
    case errMay of
        Nothing ->
            []

        Just err ->
            [ text_
                [ x "15"
                , y "15"
                , fill "red"
                ]
                [ text << toString <| err ]

            -- TODO: popup notification for that?
            ]


gameOverText : Bool -> List (Svg Msg)
gameOverText gameOverBool =
    case gameOverBool of
        False ->
            []

        True ->
            [ text_
                [ x "500"
                , y "15"
                , fill "red"
                ]
                [ text "Game Over" ]

            -- TODO: visual design for that, and present more info
            ]


nodeCircleStop : NodeXyMap -> Color -> NodeSize -> Node -> Svg Msg
nodeCircleStop nodeXyMap color size node =
    circle
        [ cx << toString <| nodeX nodeXyMap node
        , cy << toString <| nodeY nodeXyMap node
        , r (toString size)
        , fill color
        , onClick (Movement node)
        , Html.style [ ( "cursor", "pointer" ) ]
        ]
        []


nodeCircle : NodeXyMap -> Node -> Svg Msg
nodeCircle nodeXyMap node =
    Svg.g []
        [ circle
            [ cx << toString << nodeX nodeXyMap <| node
            , cy << toString << nodeY nodeXyMap <| node
            , r "20"
            , fill "#111111"
            , Svg.Attributes.cursor "pointer"
            , onClick (Movement node)
            ]
            []
        , text_
            [ x << toString <| nodeX nodeXyMap node
            , y << toString <| 5 + nodeY nodeXyMap node
            , fill "#ffffff"
            , Svg.Attributes.cursor "pointer"
            , onClick (Movement node)
            , textAnchor "middle"
            ]
            [ text << toString <| node.nodeId ]
        ]


playerCircle : GameViewDisplayInfo -> GameState -> ( Player, Node ) -> Svg Msg
playerCircle displayInfo gameState ( player, node ) =
    let
        ( x, y ) =
            positionInSvg displayInfo gameState player
    in
        circle
            [ cx << toString <| x
            , cy << toString <| y
            , r "15"
            , fill "none"
            , stroke << Maybe.withDefault "white" << AllDict.get player <| displayInfo.playerColorMap
            , Svg.Attributes.cursor "pointer"
            , onClick (Movement node)
            , strokeWidth "4"
            , Svg.Attributes.strokeDasharray "5,3.5"
            ]
            []


edgeLine : NodeXyMap -> Color -> EdgeWidth -> Edge -> Svg msg
edgeLine nodeXyMap color edgeWidth { edge } =
    let
        ( n1, n2 ) =
            edge
    in
        line
            [ x1 << toString << nodeX nodeXyMap <| n1
            , y1 << toString << nodeY nodeXyMap <| n1
            , x2 << toString << nodeX nodeXyMap <| n2
            , y2 << toString << nodeY nodeXyMap <| n2
            , strokeWidth (toString edgeWidth)
            , stroke color
            ]
            []
