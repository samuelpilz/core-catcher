module Protocol exposing (..)

import Json.Decode
import Json.Encode exposing (Value)


-- The following module comes from bartavelle/json-helpers

import Json.Helpers exposing (..)
import Dict
import Set


type alias Action =
    { player : Player
    , transport : Transport
    , node : Node
    }


jsonDecAction : Json.Decode.Decoder Action
jsonDecAction =
    ("player" := jsonDecPlayer)
        >>= \pplayer ->
                ("transport" := jsonDecTransport)
                    >>= \ptransport ->
                            ("node" := jsonDecNode)
                                >>= \pnode ->
                                        Json.Decode.succeed { player = pplayer, transport = ptransport, node = pnode }


jsonEncAction : Action -> Value
jsonEncAction val =
    Json.Encode.object
        [ ( "player", jsonEncPlayer val.player )
        , ( "transport", jsonEncTransport val.transport )
        , ( "node", jsonEncNode val.node )
        ]


type alias PlayerPositions =
    { playerPositions_ : List ( Player, Node )
    }


jsonDecPlayerPositions : Json.Decode.Decoder PlayerPositions
jsonDecPlayerPositions =
    ("playerPositions_" := Json.Decode.list (Json.Decode.map2 (,) (Json.Decode.index 0 (jsonDecPlayer)) (Json.Decode.index 1 (jsonDecNode))))
        >>= \pplayerPositions_ ->
                Json.Decode.succeed { playerPositions_ = pplayerPositions_ }


jsonEncPlayerPositions : PlayerPositions -> Value
jsonEncPlayerPositions val =
    Json.Encode.object
        [ ( "playerPositions_", (Json.Encode.list << List.map (\( v1, v2 ) -> Json.Encode.list [ (jsonEncPlayer) v1, (jsonEncNode) v2 ])) val.playerPositions_ )
        ]


type alias RogueGameView =
    { roguePlayerPositions : PlayerPositions
    , rogueEnergies : PlayerEnergies
    , rogueOwnHistory : RogueTransportHistory
    , rogueRogueLastSeen : Maybe Node
    , rogueViewError : Maybe GameError
    }


jsonDecRogueGameView : Json.Decode.Decoder RogueGameView
jsonDecRogueGameView =
    ("roguePlayerPositions" := jsonDecPlayerPositions)
        >>= \proguePlayerPositions ->
                ("rogueEnergies" := jsonDecPlayerEnergies)
                    >>= \progueEnergies ->
                            ("rogueOwnHistory" := jsonDecRogueTransportHistory)
                                >>= \progueOwnHistory ->
                                        (Json.Decode.maybe ("rogueRogueLastSeen" := jsonDecNode))
                                            >>= \progueRogueLastSeen ->
                                                    (Json.Decode.maybe ("rogueViewError" := jsonDecGameError))
                                                        >>= \progueViewError ->
                                                                Json.Decode.succeed { roguePlayerPositions = proguePlayerPositions, rogueEnergies = progueEnergies, rogueOwnHistory = progueOwnHistory, rogueRogueLastSeen = progueRogueLastSeen, rogueViewError = progueViewError }


jsonEncRogueGameView : RogueGameView -> Value
jsonEncRogueGameView val =
    Json.Encode.object
        [ ( "roguePlayerPositions", jsonEncPlayerPositions val.roguePlayerPositions )
        , ( "rogueEnergies", jsonEncPlayerEnergies val.rogueEnergies )
        , ( "rogueOwnHistory", jsonEncRogueTransportHistory val.rogueOwnHistory )
        , ( "rogueRogueLastSeen", (maybeEncode (jsonEncNode)) val.rogueRogueLastSeen )
        , ( "rogueViewError", (maybeEncode (jsonEncGameError)) val.rogueViewError )
        ]


type alias CatcherGameView =
    { catcherPlayerPositions : PlayerPositions
    , catcherEnergies : PlayerEnergies
    , catcherRogueHistory : RogueTransportHistory
    , catcherRogueLastSeen : Maybe Node
    , catcherViewError : Maybe GameError
    }


jsonDecCatcherGameView : Json.Decode.Decoder CatcherGameView
jsonDecCatcherGameView =
    ("catcherPlayerPositions" := jsonDecPlayerPositions)
        >>= \pcatcherPlayerPositions ->
                ("catcherEnergies" := jsonDecPlayerEnergies)
                    >>= \pcatcherEnergies ->
                            ("catcherRogueHistory" := jsonDecRogueTransportHistory)
                                >>= \pcatcherRogueHistory ->
                                        (Json.Decode.maybe ("catcherRogueLastSeen" := jsonDecNode))
                                            >>= \pcatcherRogueLastSeen ->
                                                    (Json.Decode.maybe ("catcherViewError" := jsonDecGameError))
                                                        >>= \pcatcherViewError ->
                                                                Json.Decode.succeed { catcherPlayerPositions = pcatcherPlayerPositions, catcherEnergies = pcatcherEnergies, catcherRogueHistory = pcatcherRogueHistory, catcherRogueLastSeen = pcatcherRogueLastSeen, catcherViewError = pcatcherViewError }


jsonEncCatcherGameView : CatcherGameView -> Value
jsonEncCatcherGameView val =
    Json.Encode.object
        [ ( "catcherPlayerPositions", jsonEncPlayerPositions val.catcherPlayerPositions )
        , ( "catcherEnergies", jsonEncPlayerEnergies val.catcherEnergies )
        , ( "catcherRogueHistory", jsonEncRogueTransportHistory val.catcherRogueHistory )
        , ( "catcherRogueLastSeen", (maybeEncode (jsonEncNode)) val.catcherRogueLastSeen )
        , ( "catcherViewError", (maybeEncode (jsonEncGameError)) val.catcherViewError )
        ]


type alias PlayerEnergies =
    { playerEnergies : List ( Player, EnergyMap )
    }


jsonDecPlayerEnergies : Json.Decode.Decoder PlayerEnergies
jsonDecPlayerEnergies =
    ("playerEnergies" := Json.Decode.list (Json.Decode.map2 (,) (Json.Decode.index 0 (jsonDecPlayer)) (Json.Decode.index 1 (jsonDecEnergyMap))))
        >>= \pplayerEnergies ->
                Json.Decode.succeed { playerEnergies = pplayerEnergies }


jsonEncPlayerEnergies : PlayerEnergies -> Value
jsonEncPlayerEnergies val =
    Json.Encode.object
        [ ( "playerEnergies", (Json.Encode.list << List.map (\( v1, v2 ) -> Json.Encode.list [ (jsonEncPlayer) v1, (jsonEncEnergyMap) v2 ])) val.playerEnergies )
        ]


type alias EnergyMap =
    { energyMap : List ( Transport, Int )
    }


jsonDecEnergyMap : Json.Decode.Decoder EnergyMap
jsonDecEnergyMap =
    ("energyMap" := Json.Decode.list (Json.Decode.map2 (,) (Json.Decode.index 0 (jsonDecTransport)) (Json.Decode.index 1 (Json.Decode.int))))
        >>= \penergyMap ->
                Json.Decode.succeed { energyMap = penergyMap }


jsonEncEnergyMap : EnergyMap -> Value
jsonEncEnergyMap val =
    Json.Encode.object
        [ ( "energyMap", (Json.Encode.list << List.map (\( v1, v2 ) -> Json.Encode.list [ (jsonEncTransport) v1, (Json.Encode.int) v2 ])) val.energyMap )
        ]


type alias Network =
    { nodes : List Node
    , overlays : List ( Transport, NetworkOverlay )
    }


jsonDecNetwork : Json.Decode.Decoder Network
jsonDecNetwork =
    ("nodes" := Json.Decode.list (jsonDecNode))
        >>= \pnodes ->
                ("overlays" := Json.Decode.list (Json.Decode.map2 (,) (Json.Decode.index 0 (jsonDecTransport)) (Json.Decode.index 1 (jsonDecNetworkOverlay))))
                    >>= \poverlays ->
                            Json.Decode.succeed { nodes = pnodes, overlays = poverlays }


jsonEncNetwork : Network -> Value
jsonEncNetwork val =
    Json.Encode.object
        [ ( "nodes", (Json.Encode.list << List.map jsonEncNode) val.nodes )
        , ( "overlays", (Json.Encode.list << List.map (\( v1, v2 ) -> Json.Encode.list [ (jsonEncTransport) v1, (jsonEncNetworkOverlay) v2 ])) val.overlays )
        ]


type alias NetworkOverlay =
    { overlayNodes : List Node
    , edges : List Edge
    }


jsonDecNetworkOverlay : Json.Decode.Decoder NetworkOverlay
jsonDecNetworkOverlay =
    ("overlayNodes" := Json.Decode.list (jsonDecNode))
        >>= \poverlayNodes ->
                ("edges" := Json.Decode.list (jsonDecEdge))
                    >>= \pedges ->
                            Json.Decode.succeed { overlayNodes = poverlayNodes, edges = pedges }


jsonEncNetworkOverlay : NetworkOverlay -> Value
jsonEncNetworkOverlay val =
    Json.Encode.object
        [ ( "overlayNodes", (Json.Encode.list << List.map jsonEncNode) val.overlayNodes )
        , ( "edges", (Json.Encode.list << List.map jsonEncEdge) val.edges )
        ]


type alias Player =
    { playerId : Int
    }


jsonDecPlayer : Json.Decode.Decoder Player
jsonDecPlayer =
    ("playerId" := Json.Decode.int)
        >>= \pplayerId ->
                Json.Decode.succeed { playerId = pplayerId }


jsonEncPlayer : Player -> Value
jsonEncPlayer val =
    Json.Encode.object
        [ ( "playerId", Json.Encode.int val.playerId )
        ]


type alias Edge =
    { edge : ( Node, Node )
    }


jsonDecEdge : Json.Decode.Decoder Edge
jsonDecEdge =
    ("edge" := Json.Decode.map2 (,) (Json.Decode.index 0 (jsonDecNode)) (Json.Decode.index 1 (jsonDecNode)))
        >>= \pedge ->
                Json.Decode.succeed { edge = pedge }


jsonEncEdge : Edge -> Value
jsonEncEdge val =
    Json.Encode.object
        [ ( "edge", (\( v1, v2 ) -> Json.Encode.list [ (jsonEncNode) v1, (jsonEncNode) v2 ]) val.edge )
        ]


type alias Node =
    { nodeId : Int
    }


jsonDecNode : Json.Decode.Decoder Node
jsonDecNode =
    ("nodeId" := Json.Decode.int)
        >>= \pnodeId ->
                Json.Decode.succeed { nodeId = pnodeId }


jsonEncNode : Node -> Value
jsonEncNode val =
    Json.Encode.object
        [ ( "nodeId", Json.Encode.int val.nodeId )
        ]


type alias Transport =
    { transportName : String
    }


jsonDecTransport : Json.Decode.Decoder Transport
jsonDecTransport =
    ("transportName" := Json.Decode.string)
        >>= \ptransportName ->
                Json.Decode.succeed { transportName = ptransportName }


jsonEncTransport : Transport -> Value
jsonEncTransport val =
    Json.Encode.object
        [ ( "transportName", Json.Encode.string val.transportName )
        ]


type alias RogueTransportHistory =
    { rogueTransportHistory : List Transport
    }


jsonDecRogueTransportHistory : Json.Decode.Decoder RogueTransportHistory
jsonDecRogueTransportHistory =
    ("rogueTransportHistory" := Json.Decode.list (jsonDecTransport))
        >>= \progueTransportHistory ->
                Json.Decode.succeed { rogueTransportHistory = progueTransportHistory }


jsonEncRogueTransportHistory : RogueTransportHistory -> Value
jsonEncRogueTransportHistory val =
    Json.Encode.object
        [ ( "rogueTransportHistory", (Json.Encode.list << List.map jsonEncTransport) val.rogueTransportHistory )
        ]


type alias GameError =
    { myError : String
    }


jsonDecGameError : Json.Decode.Decoder GameError
jsonDecGameError =
    ("myError" := Json.Decode.string)
        >>= \pmyError ->
                Json.Decode.succeed { myError = pmyError }


jsonEncGameError : GameError -> Value
jsonEncGameError val =
    Json.Encode.object
        [ ( "myError", Json.Encode.string val.myError )
        ]
