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
        [ { transportName = "underground" }
        , { transportName = "bus" }
        , { transportName = "taxi" }
        ]
    , playerColorMap = playerColorMap
    }


colorMap : ColorMap
colorMap =
    AllDict.fromList
        .transportName
        [ ( { transportName = "underground" }, "red" )
        , ( { transportName = "bus" }, "blue" )
        , ( { transportName = "taxi" }, "orange" )
        ]


edgeWidthMap : EdgeWidthMap
edgeWidthMap =
    AllDict.fromList
        .transportName
        [ ( { transportName = "underground" }, 14 )
        , ( { transportName = "bus" }, 8 )
        , ( { transportName = "taxi" }, 2 )
        ]


nodeSizeMap : EdgeWidthMap
nodeSizeMap =
    AllDict.fromList
        .transportName
        [ ( { transportName = "underground" }, 28 )
        , ( { transportName = "bus" }, 25 )
        , ( { transportName = "taxi" }, 22 )
        ]


nodeXyMap : NodeXyMap
nodeXyMap =
    AllDict.fromList
        .nodeId
        [ ( { nodeId = 1 }, ( 1, 0 ) )
        , ( { nodeId = 2 }, ( 3, 0 ) )
        , ( { nodeId = 3 }, ( 2, 2 ) )
        , ( { nodeId = 4 }, ( 2, 5 ) )
        , ( { nodeId = 5 }, ( 4, 2 ) )
        , ( { nodeId = 6 }, ( 1, 3 ) )
        , ( { nodeId = 7 }, ( 3, 4 ) )
        , ( { nodeId = 8 }, ( 5, 0 ) )
        ]


playerColorMap : PlayerColorMap
playerColorMap =
    AllDict.fromList
        .playerId
        [ ( { playerId = 1 }, "green" )
        , ( { playerId = 2 }, "yellow" )
        , ( { playerId = 3 }, "blue" )
        ]
