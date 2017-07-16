module Protocol exposing (..)

import Json.Decode
import Json.Encode exposing (Value)


-- The following module comes from bartavelle/json-helpers

import Json.Helpers exposing (..)
import EveryDict exposing (EveryDict)
import Set


arrayAsTuple2 : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder ( a, b )
arrayAsTuple2 a b =
    Json.Decode.index 0 a
        |> Json.Decode.andThen
            (\aVal ->
                Json.Decode.index 1 b
                    |> Json.Decode.andThen (\bVal -> Json.Decode.succeed ( aVal, bVal ))
            )


{-| Helper function for decoding map-like objects. It takes a decoder for the key type and a decoder for the value type.
-}
decodeMap : Json.Decode.Decoder k -> Json.Decode.Decoder v -> Json.Decode.Decoder (EveryDict k v)
decodeMap decKey decVal =
    Json.Decode.map EveryDict.fromList (Json.Decode.list <| arrayAsTuple2 decKey decVal)


{-| Helper function for encoding map-like objects. It takes an encoder for the key type and an encoder for the value type
-}
encodeMap : (k -> Json.Encode.Value) -> (v -> Json.Encode.Value) -> EveryDict k v -> Json.Encode.Value
encodeMap encKey encVal =
    Json.Encode.list << List.map (\( k, v ) -> Json.Encode.list [ encKey k, encVal v ]) << EveryDict.toList


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
    { playerPositions_ : EveryDict Player Node
    }


jsonDecPlayerPositions : Json.Decode.Decoder PlayerPositions
jsonDecPlayerPositions =
    ("playerPositions_" := decodeMap (jsonDecPlayer) (jsonDecNode))
        >>= \pplayerPositions_ ->
                Json.Decode.succeed { playerPositions_ = pplayerPositions_ }


jsonEncPlayerPositions : PlayerPositions -> Value
jsonEncPlayerPositions val =
    Json.Encode.object
        [ ( "playerPositions_", (encodeMap (jsonEncPlayer) (jsonEncNode)) val.playerPositions_ )
        ]


type alias RogueGameView =
    { roguePlayerPositions : PlayerPositions
    , rogueEnergies : PlayerEnergies
    , rogueOwnHistory : RogueHistory
    , rogueNextPlayer : Player
    }


jsonDecRogueGameView : Json.Decode.Decoder RogueGameView
jsonDecRogueGameView =
    ("roguePlayerPositions" := jsonDecPlayerPositions)
        >>= \proguePlayerPositions ->
                ("rogueEnergies" := jsonDecPlayerEnergies)
                    >>= \progueEnergies ->
                            ("rogueOwnHistory" := jsonDecRogueHistory)
                                >>= \progueOwnHistory ->
                                        ("rogueNextPlayer" := jsonDecPlayer)
                                            >>= \progueNextPlayer ->
                                                    Json.Decode.succeed { roguePlayerPositions = proguePlayerPositions, rogueEnergies = progueEnergies, rogueOwnHistory = progueOwnHistory, rogueNextPlayer = progueNextPlayer }


jsonEncRogueGameView : RogueGameView -> Value
jsonEncRogueGameView val =
    Json.Encode.object
        [ ( "roguePlayerPositions", jsonEncPlayerPositions val.roguePlayerPositions )
        , ( "rogueEnergies", jsonEncPlayerEnergies val.rogueEnergies )
        , ( "rogueOwnHistory", jsonEncRogueHistory val.rogueOwnHistory )
        , ( "rogueNextPlayer", jsonEncPlayer val.rogueNextPlayer )
        ]


type alias CatcherGameView =
    { catcherPlayerPositions : PlayerPositions
    , catcherEnergies : PlayerEnergies
    , catcherRogueHistory : RogueHistory
    , catcherNextPlayer : Player
    }


jsonDecCatcherGameView : Json.Decode.Decoder CatcherGameView
jsonDecCatcherGameView =
    ("catcherPlayerPositions" := jsonDecPlayerPositions)
        >>= \pcatcherPlayerPositions ->
                ("catcherEnergies" := jsonDecPlayerEnergies)
                    >>= \pcatcherEnergies ->
                            ("catcherRogueHistory" := jsonDecRogueHistory)
                                >>= \pcatcherRogueHistory ->
                                        ("catcherNextPlayer" := jsonDecPlayer)
                                            >>= \pcatcherNextPlayer ->
                                                    Json.Decode.succeed { catcherPlayerPositions = pcatcherPlayerPositions, catcherEnergies = pcatcherEnergies, catcherRogueHistory = pcatcherRogueHistory, catcherNextPlayer = pcatcherNextPlayer }


jsonEncCatcherGameView : CatcherGameView -> Value
jsonEncCatcherGameView val =
    Json.Encode.object
        [ ( "catcherPlayerPositions", jsonEncPlayerPositions val.catcherPlayerPositions )
        , ( "catcherEnergies", jsonEncPlayerEnergies val.catcherEnergies )
        , ( "catcherRogueHistory", jsonEncRogueHistory val.catcherRogueHistory )
        , ( "catcherNextPlayer", jsonEncPlayer val.catcherNextPlayer )
        ]


type alias PlayerEnergies =
    { playerEnergies : EveryDict Player EnergyMap
    }


jsonDecPlayerEnergies : Json.Decode.Decoder PlayerEnergies
jsonDecPlayerEnergies =
    ("playerEnergies" := decodeMap (jsonDecPlayer) (jsonDecEnergyMap))
        >>= \pplayerEnergies ->
                Json.Decode.succeed { playerEnergies = pplayerEnergies }


jsonEncPlayerEnergies : PlayerEnergies -> Value
jsonEncPlayerEnergies val =
    Json.Encode.object
        [ ( "playerEnergies", (encodeMap (jsonEncPlayer) (jsonEncEnergyMap)) val.playerEnergies )
        ]


type alias EnergyMap =
    { energyMap : EveryDict Transport Int
    }


jsonDecEnergyMap : Json.Decode.Decoder EnergyMap
jsonDecEnergyMap =
    ("energyMap" := decodeMap (jsonDecTransport) (Json.Decode.int))
        >>= \penergyMap ->
                Json.Decode.succeed { energyMap = penergyMap }


jsonEncEnergyMap : EnergyMap -> Value
jsonEncEnergyMap val =
    Json.Encode.object
        [ ( "energyMap", (encodeMap (jsonEncTransport) (Json.Encode.int)) val.energyMap )
        ]


type alias Network =
    { nodes : List Node
    , overlays : EveryDict Transport NetworkOverlay
    }


jsonDecNetwork : Json.Decode.Decoder Network
jsonDecNetwork =
    ("nodes" := Json.Decode.list (jsonDecNode))
        >>= \pnodes ->
                ("overlays" := decodeMap (jsonDecTransport) (jsonDecNetworkOverlay))
                    >>= \poverlays ->
                            Json.Decode.succeed { nodes = pnodes, overlays = poverlays }


jsonEncNetwork : Network -> Value
jsonEncNetwork val =
    Json.Encode.object
        [ ( "nodes", (Json.Encode.list << List.map jsonEncNode) val.nodes )
        , ( "overlays", (encodeMap (jsonEncTransport) (jsonEncNetworkOverlay)) val.overlays )
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


type alias RogueHistory =
    { rogueHistory_ : List ( Transport, Maybe Node )
    }


jsonDecRogueHistory : Json.Decode.Decoder RogueHistory
jsonDecRogueHistory =
    ("rogueHistory_" := Json.Decode.list (Json.Decode.map2 (,) (Json.Decode.index 0 (jsonDecTransport)) (Json.Decode.index 1 (Json.Decode.maybe (jsonDecNode)))))
        >>= \progueHistory_ ->
                Json.Decode.succeed { rogueHistory_ = progueHistory_ }


jsonEncRogueHistory : RogueHistory -> Value
jsonEncRogueHistory val =
    Json.Encode.object
        [ ( "rogueHistory_", (Json.Encode.list << List.map (\( v1, v2 ) -> Json.Encode.list [ (jsonEncTransport) v1, ((maybeEncode (jsonEncNode))) v2 ])) val.rogueHistory_ )
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
