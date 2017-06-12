module Example.ExampleGameViewDisplay exposing (..)

import Protocol exposing (..)
import GameViewDisplay exposing (..)
import List exposing (..)
import AllDict exposing (..)
import Maybe exposing (..)


displayInfo : GameViewDisplayInfo
displayInfo =
    { colorMap = colorMap
    , edgeWidthMap = edgeWidthMap
    , nodeSizeMap = nodeSizeMap
    , nodeXyMap = nodeXyMap
    , transportPriorityList =
        [ { transportName = "red" }
        , { transportName = "blue" }
        , { transportName = "orange" }
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
        .transportName
        [ ( { transportName = "red" }, "red" )
        , ( { transportName = "blue" }, "blue" )
        , ( { transportName = "orange" }, "orange" )
        ]


edgeWidthMap : EdgeWidthMap
edgeWidthMap =
    AllDict.fromList
        .transportName
        [ ( { transportName = "red" }, 14 )
        , ( { transportName = "blue" }, 8 )
        , ( { transportName = "orange" }, 2 )
        ]


nodeSizeMap : EdgeWidthMap
nodeSizeMap =
    AllDict.fromList
        .transportName
        [ ( { transportName = "red" }, 28 )
        , ( { transportName = "blue" }, 25 )
        , ( { transportName = "orange" }, 22 )
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
        .playerId
        [ ( { playerId = 0 }, "green" )
        , ( { playerId = 1 }, "yellow" )
        , ( { playerId = 2 }, "blue" )
        ]
