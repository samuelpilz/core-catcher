module Protocol exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import EveryDict exposing (EveryDict)
import Dict
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

type alias Action  =
   { actionPlayer: Player
   , actionEnergy: Energy
   , actionNode: Node
   }

jsonDecAction : Json.Decode.Decoder ( Action )
jsonDecAction =
   ("actionPlayer" := jsonDecPlayer) >>= \pactionPlayer ->
   ("actionEnergy" := jsonDecEnergy) >>= \pactionEnergy ->
   ("actionNode" := jsonDecNode) >>= \pactionNode ->
   Json.Decode.succeed {actionPlayer = pactionPlayer, actionEnergy = pactionEnergy, actionNode = pactionNode}

jsonEncAction : Action -> Value
jsonEncAction  val =
   Json.Encode.object
   [ ("actionPlayer", jsonEncPlayer val.actionPlayer)
   , ("actionEnergy", jsonEncEnergy val.actionEnergy)
   , ("actionNode", jsonEncNode val.actionNode)
   ]



type alias PlayerPositions  =
   { playerPositions: (EveryDict Player Node)
   }

jsonDecPlayerPositions : Json.Decode.Decoder ( PlayerPositions )
jsonDecPlayerPositions =
   ("playerPositions" := decodeMap (jsonDecPlayer) (jsonDecNode)) >>= \pplayerPositions ->
   Json.Decode.succeed {playerPositions = pplayerPositions}

jsonEncPlayerPositions : PlayerPositions -> Value
jsonEncPlayerPositions  val =
   Json.Encode.object
   [ ("playerPositions", (encodeMap (jsonEncPlayer) (jsonEncNode)) val.playerPositions)
   ]



type alias RogueGameView  =
   { roguePlayerPositions: PlayerPositions
   , rogueEnergies: PlayerEnergies
   , rogueOwnHistory: RogueHistory
   , rogueNextPlayer: Player
   }

jsonDecRogueGameView : Json.Decode.Decoder ( RogueGameView )
jsonDecRogueGameView =
   ("roguePlayerPositions" := jsonDecPlayerPositions) >>= \proguePlayerPositions ->
   ("rogueEnergies" := jsonDecPlayerEnergies) >>= \progueEnergies ->
   ("rogueOwnHistory" := jsonDecRogueHistory) >>= \progueOwnHistory ->
   ("rogueNextPlayer" := jsonDecPlayer) >>= \progueNextPlayer ->
   Json.Decode.succeed {roguePlayerPositions = proguePlayerPositions, rogueEnergies = progueEnergies, rogueOwnHistory = progueOwnHistory, rogueNextPlayer = progueNextPlayer}

jsonEncRogueGameView : RogueGameView -> Value
jsonEncRogueGameView  val =
   Json.Encode.object
   [ ("roguePlayerPositions", jsonEncPlayerPositions val.roguePlayerPositions)
   , ("rogueEnergies", jsonEncPlayerEnergies val.rogueEnergies)
   , ("rogueOwnHistory", jsonEncRogueHistory val.rogueOwnHistory)
   , ("rogueNextPlayer", jsonEncPlayer val.rogueNextPlayer)
   ]



type alias CatcherGameView  =
   { catcherPlayerPositions: PlayerPositions
   , catcherEnergies: PlayerEnergies
   , catcherRogueHistory: RogueHistory
   , catcherNextPlayer: Player
   }

jsonDecCatcherGameView : Json.Decode.Decoder ( CatcherGameView )
jsonDecCatcherGameView =
   ("catcherPlayerPositions" := jsonDecPlayerPositions) >>= \pcatcherPlayerPositions ->
   ("catcherEnergies" := jsonDecPlayerEnergies) >>= \pcatcherEnergies ->
   ("catcherRogueHistory" := jsonDecRogueHistory) >>= \pcatcherRogueHistory ->
   ("catcherNextPlayer" := jsonDecPlayer) >>= \pcatcherNextPlayer ->
   Json.Decode.succeed {catcherPlayerPositions = pcatcherPlayerPositions, catcherEnergies = pcatcherEnergies, catcherRogueHistory = pcatcherRogueHistory, catcherNextPlayer = pcatcherNextPlayer}

jsonEncCatcherGameView : CatcherGameView -> Value
jsonEncCatcherGameView  val =
   Json.Encode.object
   [ ("catcherPlayerPositions", jsonEncPlayerPositions val.catcherPlayerPositions)
   , ("catcherEnergies", jsonEncPlayerEnergies val.catcherEnergies)
   , ("catcherRogueHistory", jsonEncRogueHistory val.catcherRogueHistory)
   , ("catcherNextPlayer", jsonEncPlayer val.catcherNextPlayer)
   ]



type GameView  =
    RogueView RogueGameView
    | CatcherView CatcherGameView

jsonDecGameView : Json.Decode.Decoder ( GameView )
jsonDecGameView =
    let jsonDecDictGameView = Dict.fromList
            [ ("RogueView", Json.Decode.map RogueView (jsonDecRogueGameView))
            , ("CatcherView", Json.Decode.map CatcherView (jsonDecCatcherGameView))
            ]
    in  decodeSumObjectWithSingleField  "GameView" jsonDecDictGameView

jsonEncGameView : GameView -> Value
jsonEncGameView  val =
    let keyval v = case v of
                    RogueView v1 -> ("RogueView", encodeValue (jsonEncRogueGameView v1))
                    CatcherView v1 -> ("CatcherView", encodeValue (jsonEncCatcherGameView v1))
    in encodeSumObjectWithSingleField keyval val



type alias PlayerEnergies  =
   { playerEnergies: (EveryDict Player EnergyMap)
   }

jsonDecPlayerEnergies : Json.Decode.Decoder ( PlayerEnergies )
jsonDecPlayerEnergies =
   ("playerEnergies" := decodeMap (jsonDecPlayer) (jsonDecEnergyMap)) >>= \pplayerEnergies ->
   Json.Decode.succeed {playerEnergies = pplayerEnergies}

jsonEncPlayerEnergies : PlayerEnergies -> Value
jsonEncPlayerEnergies  val =
   Json.Encode.object
   [ ("playerEnergies", (encodeMap (jsonEncPlayer) (jsonEncEnergyMap)) val.playerEnergies)
   ]



type alias EnergyMap  =
   { energyMap: (EveryDict Energy Int)
   }

jsonDecEnergyMap : Json.Decode.Decoder ( EnergyMap )
jsonDecEnergyMap =
   ("energyMap" := decodeMap (jsonDecEnergy) (Json.Decode.int)) >>= \penergyMap ->
   Json.Decode.succeed {energyMap = penergyMap}

jsonEncEnergyMap : EnergyMap -> Value
jsonEncEnergyMap  val =
   Json.Encode.object
   [ ("energyMap", (encodeMap (jsonEncEnergy) (Json.Encode.int)) val.energyMap)
   ]



type alias Network  =
   { nodes: (List Node)
   , overlays: (EveryDict Energy NetworkOverlay)
   }

jsonDecNetwork : Json.Decode.Decoder ( Network )
jsonDecNetwork =
   ("nodes" := Json.Decode.list (jsonDecNode)) >>= \pnodes ->
   ("overlays" := decodeMap (jsonDecEnergy) (jsonDecNetworkOverlay)) >>= \poverlays ->
   Json.Decode.succeed {nodes = pnodes, overlays = poverlays}

jsonEncNetwork : Network -> Value
jsonEncNetwork  val =
   Json.Encode.object
   [ ("nodes", (Json.Encode.list << List.map jsonEncNode) val.nodes)
   , ("overlays", (encodeMap (jsonEncEnergy) (jsonEncNetworkOverlay)) val.overlays)
   ]



type alias NetworkOverlay  =
   { overlayNodes: (List Node)
   , overlayEdges: (List Edge)
   }

jsonDecNetworkOverlay : Json.Decode.Decoder ( NetworkOverlay )
jsonDecNetworkOverlay =
   ("overlayNodes" := Json.Decode.list (jsonDecNode)) >>= \poverlayNodes ->
   ("overlayEdges" := Json.Decode.list (jsonDecEdge)) >>= \poverlayEdges ->
   Json.Decode.succeed {overlayNodes = poverlayNodes, overlayEdges = poverlayEdges}

jsonEncNetworkOverlay : NetworkOverlay -> Value
jsonEncNetworkOverlay  val =
   Json.Encode.object
   [ ("overlayNodes", (Json.Encode.list << List.map jsonEncNode) val.overlayNodes)
   , ("overlayEdges", (Json.Encode.list << List.map jsonEncEdge) val.overlayEdges)
   ]



type alias Player  =
   { playerName: String
   }

jsonDecPlayer : Json.Decode.Decoder ( Player )
jsonDecPlayer =
   ("playerName" := Json.Decode.string) >>= \pplayerName ->
   Json.Decode.succeed {playerName = pplayerName}

jsonEncPlayer : Player -> Value
jsonEncPlayer  val =
   Json.Encode.object
   [ ("playerName", Json.Encode.string val.playerName)
   ]



type alias Edge  =
   { edge: (Node, Node)
   }

jsonDecEdge : Json.Decode.Decoder ( Edge )
jsonDecEdge =
   ("edge" := Json.Decode.map2 (,) (Json.Decode.index 0 (jsonDecNode)) (Json.Decode.index 1 (jsonDecNode))) >>= \pedge ->
   Json.Decode.succeed {edge = pedge}

jsonEncEdge : Edge -> Value
jsonEncEdge  val =
   Json.Encode.object
   [ ("edge", (\(v1,v2) -> Json.Encode.list [(jsonEncNode) v1,(jsonEncNode) v2]) val.edge)
   ]



type alias Node  =
   { nodeId: Int
   }

jsonDecNode : Json.Decode.Decoder ( Node )
jsonDecNode =
   ("nodeId" := Json.Decode.int) >>= \pnodeId ->
   Json.Decode.succeed {nodeId = pnodeId}

jsonEncNode : Node -> Value
jsonEncNode  val =
   Json.Encode.object
   [ ("nodeId", Json.Encode.int val.nodeId)
   ]



type Energy  =
    Red 
    | Blue 
    | Orange 

jsonDecEnergy : Json.Decode.Decoder ( Energy )
jsonDecEnergy = 
    let jsonDecDictEnergy = Dict.fromList [("Red", Red), ("Blue", Blue), ("Orange", Orange)]
    in  decodeSumUnaries "Energy" jsonDecDictEnergy

jsonEncEnergy : Energy -> Value
jsonEncEnergy  val =
    case val of
        Red -> Json.Encode.string "Red"
        Blue -> Json.Encode.string "Blue"
        Orange -> Json.Encode.string "Orange"



type alias RogueHistory  =
   { rogueHistory: (List (Energy, (Maybe Node)))
   }

jsonDecRogueHistory : Json.Decode.Decoder ( RogueHistory )
jsonDecRogueHistory =
   ("rogueHistory" := Json.Decode.list (Json.Decode.map2 (,) (Json.Decode.index 0 (jsonDecEnergy)) (Json.Decode.index 1 (Json.Decode.maybe (jsonDecNode))))) >>= \progueHistory ->
   Json.Decode.succeed {rogueHistory = progueHistory}

jsonEncRogueHistory : RogueHistory -> Value
jsonEncRogueHistory  val =
   Json.Encode.object
   [ ("rogueHistory", (Json.Encode.list << List.map (\(v1,v2) -> Json.Encode.list [(jsonEncEnergy) v1,((maybeEncode (jsonEncNode))) v2])) val.rogueHistory)
   ]



type alias OpenRogueHistory  =
   { openRogueHistory: (List (Energy, Node, Bool))
   }

jsonDecOpenRogueHistory : Json.Decode.Decoder ( OpenRogueHistory )
jsonDecOpenRogueHistory =
   ("openRogueHistory" := Json.Decode.list (Json.Decode.map3 (,,) (Json.Decode.index 0 (jsonDecEnergy)) (Json.Decode.index 1 (jsonDecNode)) (Json.Decode.index 2 (Json.Decode.bool)))) >>= \popenRogueHistory ->
   Json.Decode.succeed {openRogueHistory = popenRogueHistory}

jsonEncOpenRogueHistory : OpenRogueHistory -> Value
jsonEncOpenRogueHistory  val =
   Json.Encode.object
   [ ("openRogueHistory", (Json.Encode.list << List.map (\(v1,v2,v3) -> Json.Encode.list [(jsonEncEnergy) v1,(jsonEncNode) v2,(Json.Encode.bool) v3])) val.openRogueHistory)
   ]



type GameError  =
    NotTurn Player
    | PlayerNotFound Player
    | EnergyNotFound Energy
    | NotReachable Node Energy Node
    | NodeBlocked Player
    | NotEnoughEnergy 
    | GameIsOver 

jsonDecGameError : Json.Decode.Decoder ( GameError )
jsonDecGameError =
    let jsonDecDictGameError = Dict.fromList
            [ ("NotTurn", Json.Decode.map NotTurn (jsonDecPlayer))
            , ("PlayerNotFound", Json.Decode.map PlayerNotFound (jsonDecPlayer))
            , ("EnergyNotFound", Json.Decode.map EnergyNotFound (jsonDecEnergy))
            , ("NotReachable", Json.Decode.map3 NotReachable (Json.Decode.index 0 (jsonDecNode)) (Json.Decode.index 1 (jsonDecEnergy)) (Json.Decode.index 2 (jsonDecNode)))
            , ("NodeBlocked", Json.Decode.map NodeBlocked (jsonDecPlayer))
            , ("NotEnoughEnergy", Json.Decode.succeed NotEnoughEnergy)
            , ("GameIsOver", Json.Decode.succeed GameIsOver)
            ]
    in  decodeSumObjectWithSingleField  "GameError" jsonDecDictGameError

jsonEncGameError : GameError -> Value
jsonEncGameError  val =
    let keyval v = case v of
                    NotTurn v1 -> ("NotTurn", encodeValue (jsonEncPlayer v1))
                    PlayerNotFound v1 -> ("PlayerNotFound", encodeValue (jsonEncPlayer v1))
                    EnergyNotFound v1 -> ("EnergyNotFound", encodeValue (jsonEncEnergy v1))
                    NotReachable v1 v2 v3 -> ("NotReachable", encodeValue (Json.Encode.list [jsonEncNode v1, jsonEncEnergy v2, jsonEncNode v3]))
                    NodeBlocked v1 -> ("NodeBlocked", encodeValue (jsonEncPlayer v1))
                    NotEnoughEnergy  -> ("NotEnoughEnergy", encodeValue (Json.Encode.list []))
                    GameIsOver  -> ("GameIsOver", encodeValue (Json.Encode.list []))
    in encodeSumObjectWithSingleField keyval val



type alias GameOverView  =
   { gameOverViewPlayerPositions: PlayerPositions
   , gameOverViewPlayerEnergies: PlayerEnergies
   , gameOverViewRogueHistory: OpenRogueHistory
   , gameOverViewWinningPlayer: Player
   }

jsonDecGameOverView : Json.Decode.Decoder ( GameOverView )
jsonDecGameOverView =
   ("gameOverViewPlayerPositions" := jsonDecPlayerPositions) >>= \pgameOverViewPlayerPositions ->
   ("gameOverViewPlayerEnergies" := jsonDecPlayerEnergies) >>= \pgameOverViewPlayerEnergies ->
   ("gameOverViewRogueHistory" := jsonDecOpenRogueHistory) >>= \pgameOverViewRogueHistory ->
   ("gameOverViewWinningPlayer" := jsonDecPlayer) >>= \pgameOverViewWinningPlayer ->
   Json.Decode.succeed {gameOverViewPlayerPositions = pgameOverViewPlayerPositions, gameOverViewPlayerEnergies = pgameOverViewPlayerEnergies, gameOverViewRogueHistory = pgameOverViewRogueHistory, gameOverViewWinningPlayer = pgameOverViewWinningPlayer}

jsonEncGameOverView : GameOverView -> Value
jsonEncGameOverView  val =
   Json.Encode.object
   [ ("gameOverViewPlayerPositions", jsonEncPlayerPositions val.gameOverViewPlayerPositions)
   , ("gameOverViewPlayerEnergies", jsonEncPlayerEnergies val.gameOverViewPlayerEnergies)
   , ("gameOverViewRogueHistory", jsonEncOpenRogueHistory val.gameOverViewRogueHistory)
   , ("gameOverViewWinningPlayer", jsonEncPlayer val.gameOverViewWinningPlayer)
   ]



type alias InitialInfoForClient  =
   { networkForGame: Network
   , initialGameView: GameView
   , initialPlayer: Player
   , allPlayers: (List Player)
   , allEnergies: (List Energy)
   }

jsonDecInitialInfoForClient : Json.Decode.Decoder ( InitialInfoForClient )
jsonDecInitialInfoForClient =
   ("networkForGame" := jsonDecNetwork) >>= \pnetworkForGame ->
   ("initialGameView" := jsonDecGameView) >>= \pinitialGameView ->
   ("initialPlayer" := jsonDecPlayer) >>= \pinitialPlayer ->
   ("allPlayers" := Json.Decode.list (jsonDecPlayer)) >>= \pallPlayers ->
   ("allEnergies" := Json.Decode.list (jsonDecEnergy)) >>= \pallEnergies ->
   Json.Decode.succeed {networkForGame = pnetworkForGame, initialGameView = pinitialGameView, initialPlayer = pinitialPlayer, allPlayers = pallPlayers, allEnergies = pallEnergies}

jsonEncInitialInfoForClient : InitialInfoForClient -> Value
jsonEncInitialInfoForClient  val =
   Json.Encode.object
   [ ("networkForGame", jsonEncNetwork val.networkForGame)
   , ("initialGameView", jsonEncGameView val.initialGameView)
   , ("initialPlayer", jsonEncPlayer val.initialPlayer)
   , ("allPlayers", (Json.Encode.list << List.map jsonEncPlayer) val.allPlayers)
   , ("allEnergies", (Json.Encode.list << List.map jsonEncEnergy) val.allEnergies)
   ]



type MessageForServer  =
    Action_ Action
    | Login_ Login

jsonDecMessageForServer : Json.Decode.Decoder ( MessageForServer )
jsonDecMessageForServer =
    let jsonDecDictMessageForServer = Dict.fromList
            [ ("Action_", Json.Decode.map Action_ (jsonDecAction))
            , ("Login_", Json.Decode.map Login_ (jsonDecLogin))
            ]
    in  decodeSumObjectWithSingleField  "MessageForServer" jsonDecDictMessageForServer

jsonEncMessageForServer : MessageForServer -> Value
jsonEncMessageForServer  val =
    let keyval v = case v of
                    Action_ v1 -> ("Action_", encodeValue (jsonEncAction v1))
                    Login_ v1 -> ("Login_", encodeValue (jsonEncLogin v1))
    in encodeSumObjectWithSingleField keyval val



type MessageForClient  =
    ServerHello 
    | InitialInfoForClient_ InitialInfoForClient
    | GameView_ GameView
    | GameError_ GameError
    | GameOverView_ GameOverView
    | ClientMsgError 

jsonDecMessageForClient : Json.Decode.Decoder ( MessageForClient )
jsonDecMessageForClient =
    let jsonDecDictMessageForClient = Dict.fromList
            [ ("ServerHello", Json.Decode.succeed ServerHello)
            , ("InitialInfoForClient_", Json.Decode.map InitialInfoForClient_ (jsonDecInitialInfoForClient))
            , ("GameView_", Json.Decode.map GameView_ (jsonDecGameView))
            , ("GameError_", Json.Decode.map GameError_ (jsonDecGameError))
            , ("GameOverView_", Json.Decode.map GameOverView_ (jsonDecGameOverView))
            , ("ClientMsgError", Json.Decode.succeed ClientMsgError)
            ]
    in  decodeSumObjectWithSingleField  "MessageForClient" jsonDecDictMessageForClient

jsonEncMessageForClient : MessageForClient -> Value
jsonEncMessageForClient  val =
    let keyval v = case v of
                    ServerHello  -> ("ServerHello", encodeValue (Json.Encode.list []))
                    InitialInfoForClient_ v1 -> ("InitialInfoForClient_", encodeValue (jsonEncInitialInfoForClient v1))
                    GameView_ v1 -> ("GameView_", encodeValue (jsonEncGameView v1))
                    GameError_ v1 -> ("GameError_", encodeValue (jsonEncGameError v1))
                    GameOverView_ v1 -> ("GameOverView_", encodeValue (jsonEncGameOverView v1))
                    ClientMsgError  -> ("ClientMsgError", encodeValue (Json.Encode.list []))
    in encodeSumObjectWithSingleField keyval val



type alias Login  =
   { loginPlayer: Player
   }

jsonDecLogin : Json.Decode.Decoder ( Login )
jsonDecLogin =
   ("loginPlayer" := jsonDecPlayer) >>= \ploginPlayer ->
   Json.Decode.succeed {loginPlayer = ploginPlayer}

jsonEncLogin : Login -> Value
jsonEncLogin  val =
   Json.Encode.object
   [ ("loginPlayer", jsonEncPlayer val.loginPlayer)
   ]

