module Example.ExampleNetwork exposing (..)

import Protocol exposing (..)
import List exposing (..)


network : Network
network =
    { nodes = map (\n -> { nodeId = n }) (range 1 10)
    , overlays =
        [ ( {transportName = "underground"}, undergroundOverlay )
        , ( {transportName = "bus"}, busOverlay )
        , ( {transportName = "taxi"}, taxiOverlay )
        ]
    }


undergroundOverlay : NetworkOverlay
undergroundOverlay =
    { overlayNodes = [ { nodeId = 1 }, { nodeId = 5 }, { nodeId = 6 }, { nodeId = 3 } ]
    , edges =
        [ { edge = ( { nodeId = 1 }, { nodeId = 5 } ) }
        , { edge = ( { nodeId = 5 }, { nodeId = 6 } ) }
        , { edge = ( { nodeId = 1 }, { nodeId = 6 } ) }
        , { edge = ( { nodeId = 1 }, { nodeId = 3 } ) }
        ]
    }


busOverlay : NetworkOverlay
busOverlay =
    { overlayNodes = [ { nodeId = 1 }, { nodeId = 3 }, { nodeId = 4 }, { nodeId = 5 }, { nodeId = 2 }, { nodeId = 8 } ]
    , edges =
        [ { edge = ( { nodeId = 1 }, { nodeId = 3 } ) }
        , { edge = ( { nodeId = 3 }, { nodeId = 4 } ) }
        , { edge = ( { nodeId = 3 }, { nodeId = 2 } ) }
        , { edge = ( { nodeId = 1 }, { nodeId = 2 } ) }
        , { edge = ( { nodeId = 1 }, { nodeId = 5 } ) }
        , { edge = ( { nodeId = 2 }, { nodeId = 5 } ) }
        , { edge = ( { nodeId = 2 }, { nodeId = 8 } ) }
        ]
    }


taxiOverlay : NetworkOverlay
taxiOverlay =
    { overlayNodes = map (\n -> { nodeId = n }) (range 1 7 ++ range 9 10)
    , edges =
        [ { edge = ( { nodeId = 1 }, { nodeId = 2 } ) }
        , { edge = ( { nodeId = 6 }, { nodeId = 4 } ) }
        , { edge = ( { nodeId = 3 }, { nodeId = 5 } ) }
        , { edge = ( { nodeId = 2 }, { nodeId = 5 } ) }
        , { edge = ( { nodeId = 1 }, { nodeId = 6 } ) }
        , { edge = ( { nodeId = 1 }, { nodeId = 3 } ) }
        , { edge = ( { nodeId = 4 }, { nodeId = 7 } ) }
        , { edge = ( { nodeId = 5 }, { nodeId = 7 } ) }
        , { edge = ( { nodeId = 3 }, { nodeId = 6 } ) }
        , { edge = ( { nodeId = 5 }, { nodeId = 9 } ) }
        , { edge = ( { nodeId = 6 }, { nodeId = 10 } ) }
        ]
    }
