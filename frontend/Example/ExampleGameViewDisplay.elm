module Example.ExampleGameViewDisplay exposing (..)

import Protocol exposing (..)
import ProtocolUtils exposing (..)
import View.GameViewDisplay exposing (..)
import List exposing (..)
import AllDict exposing (..)
import Maybe exposing (..)


displayInfo : GameViewDisplayInfo
displayInfo =
    { colorMap = colorMap
    , edgeWidthMap = edgeWidthMap
    , nodeSizeMap = nodeSizeMap
    , nodeXyMap = nodeXyMap
    , energyPriorityList =
        [ Red
        , Blue
        , Orange
        ]
    , playerColorMap = playerColorMap
    , mapWidth = 800
    , mapHeight = 600
    , gridWidth = 7
    , gridHeight = 5
    }


colorMap : ColorMap
colorMap =
    AllDict.fromList
        energyId
        [ ( Red, "red" )
        , ( Blue, "blue" )
        , ( Orange, "orange" )
        ]


edgeWidthMap : EdgeWidthMap
edgeWidthMap =
    AllDict.fromList
        energyId
        [ ( Red, 14 )
        , ( Blue, 8 )
        , ( Orange, 2 )
        ]


nodeSizeMap : NodeSizeMap
nodeSizeMap =
    AllDict.fromList
        energyId
        [ ( Red, 28 )
        , ( Blue, 25 )
        , ( Orange, 22 )
        ]


nodeXyMap : NodeXyMap
nodeXyMap =
    AllDict.fromList
        .nodeId
        [ ( { nodeId = 1 }, ( 0, 0 ) )
        , ( { nodeId = 2 }, ( 3, 0 ) )
        , ( { nodeId = 3 }, ( 2, 2 ) )
        , ( { nodeId = 4 }, ( 2, 5 ) )
        , ( { nodeId = 5 }, ( 4, 1 ) )
        , ( { nodeId = 6 }, ( 1, 3 ) )
        , ( { nodeId = 7 }, ( 3, 4 ) )
        , ( { nodeId = 8 }, ( 5, 0 ) )
        , ( { nodeId = 9 }, ( 7, 0 ) )
        , ( { nodeId = 10 }, ( 0, 5 ) )
        , ( { nodeId = 11 }, ( 0, 2 ) )
        , ( { nodeId = 12 }, ( 6, 2 ) )
        , ( { nodeId = 13 }, ( 7, 3 ) )
        , ( { nodeId = 14 }, ( 7, 5 ) )
        , ( { nodeId = 15 }, ( 6, 5 ) )
        , ( { nodeId = 16 }, ( 5, 4 ) )
        ]


playerColorMap : PlayerColorMap
playerColorMap =
    AllDict.fromList
        .playerName
        [ ( { playerName = "Alice" }, "#4444ff" )
        , ( { playerName = "Bob" }, "red" )
        , ( { playerName = "Charlie" }, "green" )
--        , ( { playerId = 3 }, "yellow" )
        ]
