module MapView exposing (mapView)

import Html as Html
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)
import Dict exposing (..)


type alias Node =
    Int


type alias Color =
    String


type alias Edge =
    ( Node, Node, Color, Int )


type alias Network =
    ( List Node, List Edge )


mapView : Html.Html msg
mapView =
    mapView2 network


mapView2 : Network -> Html.Html msg
mapView2 ( nodes, edges ) =
    svg [ height (toString mapHeight), width (toString mapWidth), Html.Attributes.style [ ( "backgroundColor", "#cccccc" ) ] ] <|
        List.map edgeLine (sortBy (\(_,_,_,x) -> -x) edges)
            ++ List.map nodeCircle nodes
            ++ List.map nodeText nodes

mapWidth : Int
mapWidth =
    1000


mapHeight : Int
mapHeight =
    600


network : Network
network =
    ( range 1 6
    , [ ( 1, 2, "black", 2 )
      , ( 1, 3, "red", 10 )
      , ( 1, 5, "red", 10 )
      , ( 1, 5, "yellow", 2 )
      , ( 2, 3, "blue", 6 )
      , ( 2, 5, "blue", 6 )
      , ( 2, 5, "yellow", 2 )
      , ( 3, 4, "orange", 2 )
      , ( 3, 5, "yellow", 2 )
      , ( 3, 6, "red", 10 )
      , ( 3, 6, "blue", 6 )
      , ( 1, 6, "red", 10 )
      , ( 1, 6, "blue", 6 )
      , ( 1, 6, "yellow", 2 )
      ]
    )


nodePosMap : Dict Int ( Int, Int )
nodePosMap =
    fromList
        [ ( 1, ( 0, 0 ) )
        , ( 2, ( 3, 0 ) )
        , ( 3, ( 2, 2 ) )
        , ( 4, ( 2, 5 ) )
        , ( 5, ( 4, 2 ) )
        , ( 6, ( 1, 3 ) )
        ]


nodeX : Node -> Int
nodeX n =
    50
        + 100
        * (Maybe.withDefault 0
            << Maybe.map Tuple.first
            << get n
           <|
            nodePosMap
          )


nodeY : Node -> Int
nodeY n =
    50
        + 100
        * (Maybe.withDefault 0
            << Maybe.map Tuple.second
            << get n
           <|
            nodePosMap
          )



-- svg create functions


nodeCircle : Node -> Svg msg
nodeCircle n =
    circle [ cx << toString <| nodeX n, cy << toString <| nodeY n, r "20", fill "#111111" ] []


nodeText : Node -> Svg msg
nodeText n =
    text_
        [ x << toString <| -5 + nodeX n
        , y << toString <| 5 + nodeY n
        , fill "#ffffff"
        ]
        [ text (toString n) ]


edgeLine : Edge -> Svg msg
edgeLine ( n1, n2, c, width ) =
    line
        [ x1 << toString << nodeX <| n1
        , y1 << toString << nodeY <| n1
        , x2 << toString << nodeX <| n2
        , y2 << toString << nodeY <| n2
        , strokeWidth (toString width)
        , stroke c
        ]
        []
