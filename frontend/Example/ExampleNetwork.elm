module Example.ExampleNetwork exposing (..)

import Protocol exposing (..)
import List exposing (..)
import Set as Set
import Maybe exposing (..)


network : Network
network =
    { nodes = List.map (\n -> { nodeId = n }) (range 1 16)
    , overlays =
        [ ( { transportName = "red" }, redOverlay )
        , ( { transportName = "blue" }, blueOverlay )
        , ( { transportName = "orange" }, orangeOverlay )
        ]
    }


redOverlay : NetworkOverlay
redOverlay =
    mkOverlay
        [ ( 1, 6 )
        , ( 6, 13 )
        , ( 13, 9 )
        ]


blueOverlay : NetworkOverlay
blueOverlay =
    mkOverlayCompact
        [ [ 3, 4, 15, 12, 8, 3 ]
        ]


orangeOverlay : NetworkOverlay
orangeOverlay =
    mkOverlayCompact
        [ [ 6, 10, 4, 7, 16, 15, 14 ]
        , [ 1, 11, 6, 3, 5 ]
        , [ 2, 5, 8, 2 ]
        , [ 5, 12, 13, 14 ]
        , [ 8, 9 ]
        , [ 3, 7 ]
        ]


mkOverlayCompact : List (List Int) -> NetworkOverlay
mkOverlayCompact lists =
    mkOverlay <|
        List.concatMap
            (\l ->
                List.map2 (,)
                    l
                    (Maybe.withDefault [] <| tail l)
            )
            lists


mkOverlay : List ( Int, Int ) -> NetworkOverlay
mkOverlay list =
    { overlayNodes =
        List.map (\n -> { nodeId = n })
            << Set.toList
            << Set.fromList
            << unzipConcat
        <|
            list
    , edges = List.map (\( x, y ) -> { edge = ( { nodeId = x }, { nodeId = y } ) }) list
    }


unzipConcat : List ( a, a ) -> List a
unzipConcat list =
    let
        ( x, y ) =
            List.unzip list
    in
        x ++ y
