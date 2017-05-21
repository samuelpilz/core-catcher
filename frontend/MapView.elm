module MapView exposing (mapView)

import Html as Html
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)
import Dict exposing (..)
import Data exposing (..)
import Network exposing (..)
import ExampleNetwork as Example

mapView : Html.Html Msg
mapView =
    mapViewOfNetwork Example.displayInfo Example.network


mapWidth : Int
mapWidth =
    1000


mapHeight : Int
mapHeight =
    600


mapViewOfNetwork : NetworkDisplayInfo -> Network -> Html.Html Msg
mapViewOfNetwork displayInfo ( nodes, overlays ) =
    svg
        [ height (toString mapHeight)
        , width (toString mapWidth)
        , Html.Attributes.style [ ( "backgroundColor", "#cccccc" ) ]
        ]
    -- elements of svg now
    <|
        List.concatMap
            -- overlays
            (mapViewOfNetworkOverlayName displayInfo ( nodes, overlays ))
            (List.sortBy (getPriority displayInfo) << Dict.keys <| overlays)
            -- base network
            ++ List.map (nodeCircle (getNodeXyMap displayInfo)) nodes
            ++ List.map (nodeText (getNodeXyMap displayInfo)) nodes



-- takes overlay name and constructs svg elements (wraps mapViewOfNetworkOverlay)


mapViewOfNetworkOverlayName : NetworkDisplayInfo -> Network -> String -> List (Svg.Svg Msg)
mapViewOfNetworkOverlayName displayInfo ( nodes, overlays ) overlayName =
    (Maybe.withDefault []
        << Maybe.map2 (mapViewOfNetworkOverlay) (displayInfoForOverlay displayInfo overlayName)
     <|
        Dict.get overlayName overlays
    )


mapViewOfNetworkOverlay : OverlayDisplayInfo -> NetworkOverlay -> List (Svg Msg)
mapViewOfNetworkOverlay ( color, edgeWidth, nodeSize, nodeXyMap ) ( nodes, edges ) =
    List.map (edgeLine nodeXyMap color edgeWidth) edges
        ++ List.map (nodeCircleStop nodeXyMap color nodeSize) nodes



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
        , onClick (Clicked node)
        , Html.Attributes.style [ ( "cursor", "pointer" ) ]
        ]
        []


nodeText : NodeXyMap -> Node -> Svg Msg
nodeText nodeXyMap n =
    text_
        [ x << toString <| -5 + nodeX nodeXyMap n
        , y << toString <| 5 + nodeY nodeXyMap n
        , fill "#ffffff"
        , onClick (Clicked n)
        , Html.Attributes.style [ ( "cursor", "pointer" ) ]
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
