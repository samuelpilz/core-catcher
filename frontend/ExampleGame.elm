module ExampleGame exposing (..)

import Game exposing (..)
import List exposing (..)
import Dict exposing (..)
import Maybe exposing (..)


network : Network
network =
    ( (range 1 8)
    , Dict.fromList
        [ ( "underground", undergroundOverlay )
        , ( "bus", busOverlay )
        , ( "taxi", taxiOverlay )
        ]
    )


undergroundOverlay : NetworkOverlay
undergroundOverlay =
    ( [ 1, 5, 6, 3 ]
    , [ ( 1, 5 )
      , ( 5, 6 )
      , ( 1, 6 )
      , ( 1, 3 )
      ]
    )


busOverlay : NetworkOverlay
busOverlay =
    ( [ 1, 3, 4, 5, 2, 8 ]
    , [ ( 1, 3 )
      , ( 3, 4 )
      , ( 3, 2 )
      , ( 1, 2 )
      , ( 1, 5 )
      , ( 2, 5 )
      , ( 2, 8 )
      ]
    )


taxiOverlay : NetworkOverlay
taxiOverlay =
    ( range 1 7
    , [ ( 1, 2 )
      , ( 6, 4 )
      , ( 3, 5 )
      , ( 2, 5 )
      , ( 1, 6 )
      , ( 1, 3 )
      , ( 4, 7 )
      , ( 5, 7 )
      , ( 3, 6 )
      ]
    )


displayInfo : NetworkDisplayInfo
displayInfo =
    ( colorMap
    , edgeWidthMap
    , nodeSizeMap
    , nodeXyMap
    , [ "underground", "bus", "taxi" ]
    , playerColorMap
    )


colorMap : ColorMap
colorMap =
    Dict.fromList
        [ ( "underground", "red" )
        , ( "bus", "blue" )
        , ( "taxi", "orange" )
        ]


edgeWidthMap : EdgeWidthMap
edgeWidthMap =
    Dict.fromList
        [ ( "underground", 14 )
        , ( "bus", 8 )
        , ( "taxi", 2 )
        ]


nodeSizeMap : EdgeWidthMap
nodeSizeMap =
    Dict.fromList
        [ ( "underground", 28 )
        , ( "bus", 25 )
        , ( "taxi", 22 )
        ]


nodeXyMap : NodeXyMap
nodeXyMap =
    Dict.fromList
        [ ( 1, ( 1, 0 ) )
        , ( 2, ( 3, 0 ) )
        , ( 3, ( 2, 2 ) )
        , ( 4, ( 2, 5 ) )
        , ( 5, ( 4, 2 ) )
        , ( 6, ( 1, 3 ) )
        , ( 7, ( 3, 4 ) )
        , ( 8, ( 5, 0 ) )
        ]


gameState : GameState
gameState =
    Dict.fromList
        [ ( 1, 1 )
        , ( 2, 4 )
        , ( 3, 2 )
        ]


playerColorMap : PlayerColorMap
playerColorMap =
    Dict.fromList
        [ ( 1, "green" )
        , ( 2, "yellow" )
        , ( 3, "blue" )
        ]

game : Game
game = (network, displayInfo, gameState)
