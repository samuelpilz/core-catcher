module MapView exposing (mapView)

import Html as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)


type alias Node =
    Int


type alias Color =
    String


type alias Edge =
    ( Node, Node, Color )


type alias Network =
    ( List Node, List Edge )


mapView : Html.Html msg
mapView =
    mapView2 network



{- svg [ height "300", width "500" ]
   [ line [ x1 "10", y1 "10", x2 "100", y2 "100", strokeWidth "2", stroke "black" ] []
   , circle [ cx "100", cy "100", r "20", fill "#0000ff" ] []
   ]
-}


mapWidth : Int
mapWidth =
    1000


mapHeight : Int
mapHeight =
    600


network : Network
network =
    ( range 1 5
    , [ ( 1, 2, "black" )
      , ( 3, 4, "green" )
      , ( 3, 5, "yellow" )
      , ( 1, 5, "red" )
      ]
    )


mapView2 : Network -> Html.Html msg
mapView2 ( nodes, edges ) =
    svg [ height "800", width "800" ] <|
        List.map edgeLine edges
            ++ List.map nodeCircle nodes
            ++ List.map nodeText nodes


nodeX : Node -> Int
nodeX n =
    50 + ((n + 2) * 3) % 7 * (mapWidth // 7)


nodeY : Node -> Int
nodeY n =
    50 + (n * 5) % 7 * (mapHeight // 7)


nodeCircle : Node -> Svg msg
nodeCircle n =
    circle [ cx << toString <| nodeX n, cy << toString <| nodeY n, r "20", fill "#0000ff" ] []


nodeText : Node -> Svg msg
nodeText n =
    text_
        [ x << toString <| -5 + nodeX n
        , y << toString <| 5 + nodeY n
        , fill "#ffffff"
        ]
        [ text (toString n) ]


edgeLine : Edge -> Svg msg
edgeLine ( n1, n2, c ) =
    line
        [ x1 << toString << nodeX <| n1
        , y1 << toString << nodeY <| n1
        , x2 << toString << nodeX <| n2
        , y2 << toString << nodeY <| n2
        , strokeWidth "2"
        , stroke c
        ]
        []
