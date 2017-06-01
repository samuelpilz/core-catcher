module MapView exposing (mapView)

import Html as Html
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)
import Dict exposing (..)
import Data exposing (..)
import Game exposing (..)


mapWidth : Int
mapWidth =
    1000


mapHeight : Int
mapHeight =
    600


mapView : Game -> Html.Html Msg
mapView { network, displayInfo, gameState } =
    svg
        [ height (toString mapHeight)
        , width (toString mapWidth)
        , Html.Attributes.style [ ( "backgroundColor", "#cccccc" ) ]
        ]
    -- elements of svg now
    <|
        List.concatMap
            -- overlays
            (mapViewOfNetworkOverlayName displayInfo network)
            (List.sortBy (\( name, _ ) -> getPriority displayInfo name) <| Dict.toList network.overlays)
            -- base network
            ++ List.map (nodeCircle displayInfo.nodeXyMap) network.nodes
            ++ List.map (playerCircle displayInfo.nodeXyMap) (Dict.toList gameState)
            ++ List.map (nodeText displayInfo.nodeXyMap) network.nodes


mapViewOfNetworkOverlayName : NetworkDisplayInfo -> Network -> ( String, NetworkOverlay ) -> List (Svg.Svg Msg)
mapViewOfNetworkOverlayName displayInfo { overlays } ( overlayName, overlay ) =
    (Maybe.withDefault []
        << Maybe.map2 (mapViewOfNetworkOverlay)
            (displayInfoForTransport displayInfo overlayName)
     <|
        Just overlay
    )


mapViewOfNetworkOverlay : OverlayDisplayInfo -> NetworkOverlay -> List (Svg Msg)
mapViewOfNetworkOverlay { color, edgeWidth, nodeSize, nodeXyMap } { overlayNodes, edges } =
    List.map (edgeLine nodeXyMap color edgeWidth) edges
        ++ List.map (nodeCircleStop nodeXyMap color nodeSize) overlayNodes



-- svg create functions


nodeCircleStop : NodeXyMap -> Color -> NodeSize -> Node -> Svg Msg
nodeCircleStop nodeXyMap color size node =
    circle
        [ cx << toString <| nodeX nodeXyMap node
        , cy << toString <| nodeY nodeXyMap node
        , r (toString size)
        , fill color
        , onClick (Clicked node)
        , Html.Attributes.style [ ( "cursor", "pointer" ) ]
        ]
        []


nodeCircle : NodeXyMap -> Node -> Svg Msg
nodeCircle nodeXyMap node =
    circle
        [ cx << toString << nodeX nodeXyMap <| node
        , cy << toString << nodeY nodeXyMap <| node
        , r "20"
        , fill "#111111"
        , Svg.Attributes.cursor "pointer"
        , onClick (Clicked node)
        ]
        []


playerCircle : NodeXyMap -> ( Player, Node ) -> Svg Msg
playerCircle nodeXyMap ( player, node ) =
    circle
        [ cx << toString << nodeX nodeXyMap <| node
        , cy << toString << nodeY nodeXyMap <| node
        , r "15"
        , fill "none"
        , stroke
            (if player == 1 then
                "yellow"
             else if player == 2 then
                "green"
             else
                "white"
            )
        , Svg.Attributes.cursor "pointer"
        , onClick (Clicked node)
        , Svg.Attributes.strokeDasharray "5,5"
        ]
        []


nodeText : NodeXyMap -> Node -> Svg Msg
nodeText nodeXyMap n =
    text_
        [ x << toString <| -5 + nodeX nodeXyMap n
        , y << toString <| 5 + nodeY nodeXyMap n
        , fill "#ffffff"
        , Svg.Attributes.cursor "pointer"
        , onClick (Clicked n)
        ]
        [ text (toString n) ]


edgeLine : NodeXyMap -> Color -> EdgeWidth -> Edge -> Svg msg
edgeLine nodeXyMap color edgeWidth ( n1, n2 ) =
    line
        [ x1 << toString << nodeX nodeXyMap <| n1
        , y1 << toString << nodeY nodeXyMap <| n1
        , x2 << toString << nodeX nodeXyMap <| n2
        , y2 << toString << nodeY nodeXyMap <| n2
        , strokeWidth (toString edgeWidth)
        , stroke color
        ]
        []
