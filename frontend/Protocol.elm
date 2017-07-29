module Protocol exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict
import Set


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
   { playerPositions: (List (Player, Node))
   }

jsonDecPlayerPositions : Json.Decode.Decoder ( PlayerPositions )
jsonDecPlayerPositions =
   ("playerPositions" := Json.Decode.list (Json.Decode.map2 (,) (Json.Decode.index 0 (jsonDecPlayer)) (Json.Decode.index 1 (jsonDecNode)))) >>= \pplayerPositions ->
   Json.Decode.succeed {playerPositions = pplayerPositions}

jsonEncPlayerPositions : PlayerPositions -> Value
jsonEncPlayerPositions  val =
   Json.Encode.object
   [ ("playerPositions", (Json.Encode.list << List.map (\(v1,v2) -> Json.Encode.list [(jsonEncPlayer) v1,(jsonEncNode) v2])) val.playerPositions)
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
   { playerEnergies: (List (Player, EnergyMap))
   }

jsonDecPlayerEnergies : Json.Decode.Decoder ( PlayerEnergies )
jsonDecPlayerEnergies =
   ("playerEnergies" := Json.Decode.list (Json.Decode.map2 (,) (Json.Decode.index 0 (jsonDecPlayer)) (Json.Decode.index 1 (jsonDecEnergyMap)))) >>= \pplayerEnergies ->
   Json.Decode.succeed {playerEnergies = pplayerEnergies}

jsonEncPlayerEnergies : PlayerEnergies -> Value
jsonEncPlayerEnergies  val =
   Json.Encode.object
   [ ("playerEnergies", (Json.Encode.list << List.map (\(v1,v2) -> Json.Encode.list [(jsonEncPlayer) v1,(jsonEncEnergyMap) v2])) val.playerEnergies)
   ]



type alias EnergyMap  =
   { energyMap: (List (Energy, Int))
   }

jsonDecEnergyMap : Json.Decode.Decoder ( EnergyMap )
jsonDecEnergyMap =
   ("energyMap" := Json.Decode.list (Json.Decode.map2 (,) (Json.Decode.index 0 (jsonDecEnergy)) (Json.Decode.index 1 (Json.Decode.int)))) >>= \penergyMap ->
   Json.Decode.succeed {energyMap = penergyMap}

jsonEncEnergyMap : EnergyMap -> Value
jsonEncEnergyMap  val =
   Json.Encode.object
   [ ("energyMap", (Json.Encode.list << List.map (\(v1,v2) -> Json.Encode.list [(jsonEncEnergy) v1,(Json.Encode.int) v2])) val.energyMap)
   ]



type alias Network  =
   { nodes: (List Node)
   , overlays: (List (Energy, NetworkOverlay))
   }

jsonDecNetwork : Json.Decode.Decoder ( Network )
jsonDecNetwork =
   ("nodes" := Json.Decode.list (jsonDecNode)) >>= \pnodes ->
   ("overlays" := Json.Decode.list (Json.Decode.map2 (,) (Json.Decode.index 0 (jsonDecEnergy)) (Json.Decode.index 1 (jsonDecNetworkOverlay)))) >>= \poverlays ->
   Json.Decode.succeed {nodes = pnodes, overlays = poverlays}

jsonEncNetwork : Network -> Value
jsonEncNetwork  val =
   Json.Encode.object
   [ ("nodes", (Json.Encode.list << List.map jsonEncNode) val.nodes)
   , ("overlays", (Json.Encode.list << List.map (\(v1,v2) -> Json.Encode.list [(jsonEncEnergy) v1,(jsonEncNetworkOverlay) v2])) val.overlays)
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
   { playerId: Int
   }

jsonDecPlayer : Json.Decode.Decoder ( Player )
jsonDecPlayer =
   ("playerId" := Json.Decode.int) >>= \pplayerId ->
   Json.Decode.succeed {playerId = pplayerId}

jsonEncPlayer : Player -> Value
jsonEncPlayer  val =
   Json.Encode.object
   [ ("playerId", Json.Encode.int val.playerId)
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
    NotTurn 
    | PlayerNotFound Player
    | EnergyNotFound 
    | NotReachable 
    | NodeBlocked Player
    | NotEnoughEnergy 
    | GameIsOver 

jsonDecGameError : Json.Decode.Decoder ( GameError )
jsonDecGameError =
    let jsonDecDictGameError = Dict.fromList
            [ ("NotTurn", Json.Decode.succeed NotTurn)
            , ("PlayerNotFound", Json.Decode.map PlayerNotFound (jsonDecPlayer))
            , ("EnergyNotFound", Json.Decode.succeed EnergyNotFound)
            , ("NotReachable", Json.Decode.succeed NotReachable)
            , ("NodeBlocked", Json.Decode.map NodeBlocked (jsonDecPlayer))
            , ("NotEnoughEnergy", Json.Decode.succeed NotEnoughEnergy)
            , ("GameIsOver", Json.Decode.succeed GameIsOver)
            ]
    in  decodeSumObjectWithSingleField  "GameError" jsonDecDictGameError

jsonEncGameError : GameError -> Value
jsonEncGameError  val =
    let keyval v = case v of
                    NotTurn  -> ("NotTurn", encodeValue (Json.Encode.list []))
                    PlayerNotFound v1 -> ("PlayerNotFound", encodeValue (jsonEncPlayer v1))
                    EnergyNotFound  -> ("EnergyNotFound", encodeValue (Json.Encode.list []))
                    NotReachable  -> ("NotReachable", encodeValue (Json.Encode.list []))
                    NodeBlocked v1 -> ("NodeBlocked", encodeValue (jsonEncPlayer v1))
                    NotEnoughEnergy  -> ("NotEnoughEnergy", encodeValue (Json.Encode.list []))
                    GameIsOver  -> ("GameIsOver", encodeValue (Json.Encode.list []))
    in encodeSumObjectWithSingleField keyval val



type alias GameOverView  =
   { gameOverViewPlayerPositions: PlayerPositions
   , gameOverViewEnergies: PlayerEnergies
   , gameOverViewRogueHistory: OpenRogueHistory
   }

jsonDecGameOverView : Json.Decode.Decoder ( GameOverView )
jsonDecGameOverView =
   ("gameOverViewPlayerPositions" := jsonDecPlayerPositions) >>= \pgameOverViewPlayerPositions ->
   ("gameOverViewEnergies" := jsonDecPlayerEnergies) >>= \pgameOverViewEnergies ->
   ("gameOverViewRogueHistory" := jsonDecOpenRogueHistory) >>= \pgameOverViewRogueHistory ->
   Json.Decode.succeed {gameOverViewPlayerPositions = pgameOverViewPlayerPositions, gameOverViewEnergies = pgameOverViewEnergies, gameOverViewRogueHistory = pgameOverViewRogueHistory}

jsonEncGameOverView : GameOverView -> Value
jsonEncGameOverView  val =
   Json.Encode.object
   [ ("gameOverViewPlayerPositions", jsonEncPlayerPositions val.gameOverViewPlayerPositions)
   , ("gameOverViewEnergies", jsonEncPlayerEnergies val.gameOverViewEnergies)
   , ("gameOverViewRogueHistory", jsonEncOpenRogueHistory val.gameOverViewRogueHistory)
   ]



type alias InitialInfoForClient  =
   { initialPlayer: Player
   , networkForGame: Network
   , initialGameView: GameView
   }

jsonDecInitialInfoForClient : Json.Decode.Decoder ( InitialInfoForClient )
jsonDecInitialInfoForClient =
   ("initialPlayer" := jsonDecPlayer) >>= \pinitialPlayer ->
   ("networkForGame" := jsonDecNetwork) >>= \pnetworkForGame ->
   ("initialGameView" := jsonDecGameView) >>= \pinitialGameView ->
   Json.Decode.succeed {initialPlayer = pinitialPlayer, networkForGame = pnetworkForGame, initialGameView = pinitialGameView}

jsonEncInitialInfoForClient : InitialInfoForClient -> Value
jsonEncInitialInfoForClient  val =
   Json.Encode.object
   [ ("initialPlayer", jsonEncPlayer val.initialPlayer)
   , ("networkForGame", jsonEncNetwork val.networkForGame)
   , ("initialGameView", jsonEncGameView val.initialGameView)
   ]



type MessageForServer  =
    Action_ Action

jsonDecMessageForServer : Json.Decode.Decoder ( MessageForServer )
jsonDecMessageForServer =
    Json.Decode.map Action_ (jsonDecAction)


jsonEncMessageForServer : MessageForServer -> Value
jsonEncMessageForServer (Action_ v1) =
    jsonEncAction v1



type MessageForClient  =
    GameView_ GameView
    | GameError_ GameError
    | GameOverView_ GameOverView
    | InitialInfoForClient_ InitialInfoForClient

jsonDecMessageForClient : Json.Decode.Decoder ( MessageForClient )
jsonDecMessageForClient =
    let jsonDecDictMessageForClient = Dict.fromList
            [ ("GameView_", Json.Decode.map GameView_ (jsonDecGameView))
            , ("GameError_", Json.Decode.map GameError_ (jsonDecGameError))
            , ("GameOverView_", Json.Decode.map GameOverView_ (jsonDecGameOverView))
            , ("InitialInfoForClient_", Json.Decode.map InitialInfoForClient_ (jsonDecInitialInfoForClient))
            ]
    in  decodeSumObjectWithSingleField  "MessageForClient" jsonDecDictMessageForClient

jsonEncMessageForClient : MessageForClient -> Value
jsonEncMessageForClient  val =
    let keyval v = case v of
                    GameView_ v1 -> ("GameView_", encodeValue (jsonEncGameView v1))
                    GameError_ v1 -> ("GameError_", encodeValue (jsonEncGameError v1))
                    GameOverView_ v1 -> ("GameOverView_", encodeValue (jsonEncGameOverView v1))
                    InitialInfoForClient_ v1 -> ("InitialInfoForClient_", encodeValue (jsonEncInitialInfoForClient v1))
    in encodeSumObjectWithSingleField keyval val

