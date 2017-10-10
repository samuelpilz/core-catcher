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
   , rogueOwnHistory: ShadowRogueHistory
   , rogueNextPlayer: Player
   }

jsonDecRogueGameView : Json.Decode.Decoder ( RogueGameView )
jsonDecRogueGameView =
   ("roguePlayerPositions" := jsonDecPlayerPositions) >>= \proguePlayerPositions ->
   ("rogueEnergies" := jsonDecPlayerEnergies) >>= \progueEnergies ->
   ("rogueOwnHistory" := jsonDecShadowRogueHistory) >>= \progueOwnHistory ->
   ("rogueNextPlayer" := jsonDecPlayer) >>= \progueNextPlayer ->
   Json.Decode.succeed {roguePlayerPositions = proguePlayerPositions, rogueEnergies = progueEnergies, rogueOwnHistory = progueOwnHistory, rogueNextPlayer = progueNextPlayer}

jsonEncRogueGameView : RogueGameView -> Value
jsonEncRogueGameView  val =
   Json.Encode.object
   [ ("roguePlayerPositions", jsonEncPlayerPositions val.roguePlayerPositions)
   , ("rogueEnergies", jsonEncPlayerEnergies val.rogueEnergies)
   , ("rogueOwnHistory", jsonEncShadowRogueHistory val.rogueOwnHistory)
   , ("rogueNextPlayer", jsonEncPlayer val.rogueNextPlayer)
   ]



type alias CatcherGameView  =
   { catcherPlayerPositions: PlayerPositions
   , catcherEnergies: PlayerEnergies
   , catcherRogueHistory: ShadowRogueHistory
   , catcherNextPlayer: Player
   }

jsonDecCatcherGameView : Json.Decode.Decoder ( CatcherGameView )
jsonDecCatcherGameView =
   ("catcherPlayerPositions" := jsonDecPlayerPositions) >>= \pcatcherPlayerPositions ->
   ("catcherEnergies" := jsonDecPlayerEnergies) >>= \pcatcherEnergies ->
   ("catcherRogueHistory" := jsonDecShadowRogueHistory) >>= \pcatcherRogueHistory ->
   ("catcherNextPlayer" := jsonDecPlayer) >>= \pcatcherNextPlayer ->
   Json.Decode.succeed {catcherPlayerPositions = pcatcherPlayerPositions, catcherEnergies = pcatcherEnergies, catcherRogueHistory = pcatcherRogueHistory, catcherNextPlayer = pcatcherNextPlayer}

jsonEncCatcherGameView : CatcherGameView -> Value
jsonEncCatcherGameView  val =
   Json.Encode.object
   [ ("catcherPlayerPositions", jsonEncPlayerPositions val.catcherPlayerPositions)
   , ("catcherEnergies", jsonEncPlayerEnergies val.catcherEnergies)
   , ("catcherRogueHistory", jsonEncShadowRogueHistory val.catcherRogueHistory)
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



type alias GameId  =
   { gameId: Int
   }

jsonDecGameId : Json.Decode.Decoder ( GameId )
jsonDecGameId =
   ("gameId" := Json.Decode.int) >>= \pgameId ->
   Json.Decode.succeed {gameId = pgameId}

jsonEncGameId : GameId -> Value
jsonEncGameId  val =
   Json.Encode.object
   [ ("gameId", Json.Encode.int val.gameId)
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



type alias ShadowRogueHistory  =
   { shadowRogueHistory: (List (Energy, (Maybe Node)))
   }

jsonDecShadowRogueHistory : Json.Decode.Decoder ( ShadowRogueHistory )
jsonDecShadowRogueHistory =
   ("shadowRogueHistory" := Json.Decode.list (Json.Decode.map2 (,) (Json.Decode.index 0 (jsonDecEnergy)) (Json.Decode.index 1 (Json.Decode.maybe (jsonDecNode))))) >>= \pshadowRogueHistory ->
   Json.Decode.succeed {shadowRogueHistory = pshadowRogueHistory}

jsonEncShadowRogueHistory : ShadowRogueHistory -> Value
jsonEncShadowRogueHistory  val =
   Json.Encode.object
   [ ("shadowRogueHistory", (Json.Encode.list << List.map (\(v1,v2) -> Json.Encode.list [(jsonEncEnergy) v1,((maybeEncode (jsonEncNode))) v2])) val.shadowRogueHistory)
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



type RogueHistory  =
    OpenHistory OpenRogueHistory
    | ShadowHistory ShadowRogueHistory

jsonDecRogueHistory : Json.Decode.Decoder ( RogueHistory )
jsonDecRogueHistory =
    let jsonDecDictRogueHistory = Dict.fromList
            [ ("OpenHistory", Json.Decode.map OpenHistory (jsonDecOpenRogueHistory))
            , ("ShadowHistory", Json.Decode.map ShadowHistory (jsonDecShadowRogueHistory))
            ]
    in  decodeSumObjectWithSingleField  "RogueHistory" jsonDecDictRogueHistory

jsonEncRogueHistory : RogueHistory -> Value
jsonEncRogueHistory  val =
    let keyval v = case v of
                    OpenHistory v1 -> ("OpenHistory", encodeValue (jsonEncOpenRogueHistory v1))
                    ShadowHistory v1 -> ("ShadowHistory", encodeValue (jsonEncShadowRogueHistory v1))
    in encodeSumObjectWithSingleField keyval val



type GameError  =
    NotTurn Player
    | PlayerNotFound Player
    | EnergyNotFound Energy
    | NotReachable Node Energy Node
    | NodeBlocked Player
    | NotEnoughEnergy 
    | GameIsOver 
    | GameNotStarted 

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
            , ("GameNotStarted", Json.Decode.succeed GameNotStarted)
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
                    GameNotStarted  -> ("GameNotStarted", encodeValue (Json.Encode.list []))
    in encodeSumObjectWithSingleField keyval val



type alias GameOverView  =
   { gameOverViewPlayerPositions: PlayerPositions
   , gameOverViewPlayerEnergies: PlayerEnergies
   , gameOverViewRogueHistory: OpenRogueHistory
   , gameOverViewWinningPlayer: Player
   , gameOverViewNetwork: Network
   , gameOverViewAllPlayers: (List Player)
   , gameOverViewAllEnergies: (List Energy)
   , gameOverViewGameName: String
   }

jsonDecGameOverView : Json.Decode.Decoder ( GameOverView )
jsonDecGameOverView =
   ("gameOverViewPlayerPositions" := jsonDecPlayerPositions) >>= \pgameOverViewPlayerPositions ->
   ("gameOverViewPlayerEnergies" := jsonDecPlayerEnergies) >>= \pgameOverViewPlayerEnergies ->
   ("gameOverViewRogueHistory" := jsonDecOpenRogueHistory) >>= \pgameOverViewRogueHistory ->
   ("gameOverViewWinningPlayer" := jsonDecPlayer) >>= \pgameOverViewWinningPlayer ->
   ("gameOverViewNetwork" := jsonDecNetwork) >>= \pgameOverViewNetwork ->
   ("gameOverViewAllPlayers" := Json.Decode.list (jsonDecPlayer)) >>= \pgameOverViewAllPlayers ->
   ("gameOverViewAllEnergies" := Json.Decode.list (jsonDecEnergy)) >>= \pgameOverViewAllEnergies ->
   ("gameOverViewGameName" := Json.Decode.string) >>= \pgameOverViewGameName ->
   Json.Decode.succeed {gameOverViewPlayerPositions = pgameOverViewPlayerPositions, gameOverViewPlayerEnergies = pgameOverViewPlayerEnergies, gameOverViewRogueHistory = pgameOverViewRogueHistory, gameOverViewWinningPlayer = pgameOverViewWinningPlayer, gameOverViewNetwork = pgameOverViewNetwork, gameOverViewAllPlayers = pgameOverViewAllPlayers, gameOverViewAllEnergies = pgameOverViewAllEnergies, gameOverViewGameName = pgameOverViewGameName}

jsonEncGameOverView : GameOverView -> Value
jsonEncGameOverView  val =
   Json.Encode.object
   [ ("gameOverViewPlayerPositions", jsonEncPlayerPositions val.gameOverViewPlayerPositions)
   , ("gameOverViewPlayerEnergies", jsonEncPlayerEnergies val.gameOverViewPlayerEnergies)
   , ("gameOverViewRogueHistory", jsonEncOpenRogueHistory val.gameOverViewRogueHistory)
   , ("gameOverViewWinningPlayer", jsonEncPlayer val.gameOverViewWinningPlayer)
   , ("gameOverViewNetwork", jsonEncNetwork val.gameOverViewNetwork)
   , ("gameOverViewAllPlayers", (Json.Encode.list << List.map jsonEncPlayer) val.gameOverViewAllPlayers)
   , ("gameOverViewAllEnergies", (Json.Encode.list << List.map jsonEncEnergy) val.gameOverViewAllEnergies)
   , ("gameOverViewGameName", Json.Encode.string val.gameOverViewGameName)
   ]



type alias InitialInfoGameActive  =
   { networkForGame: Network
   , initialGameView: GameView
   , startingPlayer: Player
   , initialInfoAllPlayers: (List Player)
   , initialInfoAllEnergies: (List Energy)
   , initialInfoGameName: String
   }

jsonDecInitialInfoGameActive : Json.Decode.Decoder ( InitialInfoGameActive )
jsonDecInitialInfoGameActive =
   ("networkForGame" := jsonDecNetwork) >>= \pnetworkForGame ->
   ("initialGameView" := jsonDecGameView) >>= \pinitialGameView ->
   ("startingPlayer" := jsonDecPlayer) >>= \pstartingPlayer ->
   ("initialInfoAllPlayers" := Json.Decode.list (jsonDecPlayer)) >>= \pinitialInfoAllPlayers ->
   ("initialInfoAllEnergies" := Json.Decode.list (jsonDecEnergy)) >>= \pinitialInfoAllEnergies ->
   ("initialInfoGameName" := Json.Decode.string) >>= \pinitialInfoGameName ->
   Json.Decode.succeed {networkForGame = pnetworkForGame, initialGameView = pinitialGameView, startingPlayer = pstartingPlayer, initialInfoAllPlayers = pinitialInfoAllPlayers, initialInfoAllEnergies = pinitialInfoAllEnergies, initialInfoGameName = pinitialInfoGameName}

jsonEncInitialInfoGameActive : InitialInfoGameActive -> Value
jsonEncInitialInfoGameActive  val =
   Json.Encode.object
   [ ("networkForGame", jsonEncNetwork val.networkForGame)
   , ("initialGameView", jsonEncGameView val.initialGameView)
   , ("startingPlayer", jsonEncPlayer val.startingPlayer)
   , ("initialInfoAllPlayers", (Json.Encode.list << List.map jsonEncPlayer) val.initialInfoAllPlayers)
   , ("initialInfoAllEnergies", (Json.Encode.list << List.map jsonEncEnergy) val.initialInfoAllEnergies)
   , ("initialInfoGameName", Json.Encode.string val.initialInfoGameName)
   ]



type MessageForServer  =
    Login_ Login
    | CreateNewGame_ CreateNewGame
    | Action_ Action
    | StartGame 
    | JoinGame_ JoinGame
    | PlayerHomeRefresh 

jsonDecMessageForServer : Json.Decode.Decoder ( MessageForServer )
jsonDecMessageForServer =
    let jsonDecDictMessageForServer = Dict.fromList
            [ ("Login_", Json.Decode.map Login_ (jsonDecLogin))
            , ("CreateNewGame_", Json.Decode.map CreateNewGame_ (jsonDecCreateNewGame))
            , ("Action_", Json.Decode.map Action_ (jsonDecAction))
            , ("StartGame", Json.Decode.succeed StartGame)
            , ("JoinGame_", Json.Decode.map JoinGame_ (jsonDecJoinGame))
            , ("PlayerHomeRefresh", Json.Decode.succeed PlayerHomeRefresh)
            ]
    in  decodeSumObjectWithSingleField  "MessageForServer" jsonDecDictMessageForServer

jsonEncMessageForServer : MessageForServer -> Value
jsonEncMessageForServer  val =
    let keyval v = case v of
                    Login_ v1 -> ("Login_", encodeValue (jsonEncLogin v1))
                    CreateNewGame_ v1 -> ("CreateNewGame_", encodeValue (jsonEncCreateNewGame v1))
                    Action_ v1 -> ("Action_", encodeValue (jsonEncAction v1))
                    StartGame  -> ("StartGame", encodeValue (Json.Encode.list []))
                    JoinGame_ v1 -> ("JoinGame_", encodeValue (jsonEncJoinGame v1))
                    PlayerHomeRefresh  -> ("PlayerHomeRefresh", encodeValue (Json.Encode.list []))
    in encodeSumObjectWithSingleField keyval val



type MessageForClient  =
    ServerHello 
    | LoginFail_ LoginFail
    | PlayerHome_ PlayerHome
    | InitialInfoGameActive_ InitialInfoGameActive
    | GameView_ GameView
    | GameOverView_ GameOverView
    | GameLobbyView_ GameLobbyView
    | ServerError_ ServerError

jsonDecMessageForClient : Json.Decode.Decoder ( MessageForClient )
jsonDecMessageForClient =
    let jsonDecDictMessageForClient = Dict.fromList
            [ ("ServerHello", Json.Decode.succeed ServerHello)
            , ("LoginFail_", Json.Decode.map LoginFail_ (jsonDecLoginFail))
            , ("PlayerHome_", Json.Decode.map PlayerHome_ (jsonDecPlayerHome))
            , ("InitialInfoGameActive_", Json.Decode.map InitialInfoGameActive_ (jsonDecInitialInfoGameActive))
            , ("GameView_", Json.Decode.map GameView_ (jsonDecGameView))
            , ("GameOverView_", Json.Decode.map GameOverView_ (jsonDecGameOverView))
            , ("GameLobbyView_", Json.Decode.map GameLobbyView_ (jsonDecGameLobbyView))
            , ("ServerError_", Json.Decode.map ServerError_ (jsonDecServerError))
            ]
    in  decodeSumObjectWithSingleField  "MessageForClient" jsonDecDictMessageForClient

jsonEncMessageForClient : MessageForClient -> Value
jsonEncMessageForClient  val =
    let keyval v = case v of
                    ServerHello  -> ("ServerHello", encodeValue (Json.Encode.list []))
                    LoginFail_ v1 -> ("LoginFail_", encodeValue (jsonEncLoginFail v1))
                    PlayerHome_ v1 -> ("PlayerHome_", encodeValue (jsonEncPlayerHome v1))
                    InitialInfoGameActive_ v1 -> ("InitialInfoGameActive_", encodeValue (jsonEncInitialInfoGameActive v1))
                    GameView_ v1 -> ("GameView_", encodeValue (jsonEncGameView v1))
                    GameOverView_ v1 -> ("GameOverView_", encodeValue (jsonEncGameOverView v1))
                    GameLobbyView_ v1 -> ("GameLobbyView_", encodeValue (jsonEncGameLobbyView v1))
                    ServerError_ v1 -> ("ServerError_", encodeValue (jsonEncServerError v1))
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



type alias JoinGame  =
   { joinGameId: GameId
   }

jsonDecJoinGame : Json.Decode.Decoder ( JoinGame )
jsonDecJoinGame =
   ("joinGameId" := jsonDecGameId) >>= \pjoinGameId ->
   Json.Decode.succeed {joinGameId = pjoinGameId}

jsonEncJoinGame : JoinGame -> Value
jsonEncJoinGame  val =
   Json.Encode.object
   [ ("joinGameId", jsonEncGameId val.joinGameId)
   ]



type alias LoginFail  =
   { loginFailPlayer: Player
   }

jsonDecLoginFail : Json.Decode.Decoder ( LoginFail )
jsonDecLoginFail =
   ("loginFailPlayer" := jsonDecPlayer) >>= \ploginFailPlayer ->
   Json.Decode.succeed {loginFailPlayer = ploginFailPlayer}

jsonEncLoginFail : LoginFail -> Value
jsonEncLoginFail  val =
   Json.Encode.object
   [ ("loginFailPlayer", jsonEncPlayer val.loginFailPlayer)
   ]



type alias PlayerHome  =
   { playerHomePlayer: Player
   , activeGames: (List GamePreview)
   , activeLobbies: (List GameLobbyPreview)
   }

jsonDecPlayerHome : Json.Decode.Decoder ( PlayerHome )
jsonDecPlayerHome =
   ("playerHomePlayer" := jsonDecPlayer) >>= \pplayerHomePlayer ->
   ("activeGames" := Json.Decode.list (jsonDecGamePreview)) >>= \pactiveGames ->
   ("activeLobbies" := Json.Decode.list (jsonDecGameLobbyPreview)) >>= \pactiveLobbies ->
   Json.Decode.succeed {playerHomePlayer = pplayerHomePlayer, activeGames = pactiveGames, activeLobbies = pactiveLobbies}

jsonEncPlayerHome : PlayerHome -> Value
jsonEncPlayerHome  val =
   Json.Encode.object
   [ ("playerHomePlayer", jsonEncPlayer val.playerHomePlayer)
   , ("activeGames", (Json.Encode.list << List.map jsonEncGamePreview) val.activeGames)
   , ("activeLobbies", (Json.Encode.list << List.map jsonEncGameLobbyPreview) val.activeLobbies)
   ]



type alias CreateNewGame  =
   { createGameName: String
   }

jsonDecCreateNewGame : Json.Decode.Decoder ( CreateNewGame )
jsonDecCreateNewGame =
   ("createGameName" := Json.Decode.string) >>= \pcreateGameName ->
   Json.Decode.succeed {createGameName = pcreateGameName}

jsonEncCreateNewGame : CreateNewGame -> Value
jsonEncCreateNewGame  val =
   Json.Encode.object
   [ ("createGameName", Json.Encode.string val.createGameName)
   ]



type alias GameLobbyView  =
   { gameLobbyViewGameName: String
   , gameLobbyViewPlayers: (List Player)
   }

jsonDecGameLobbyView : Json.Decode.Decoder ( GameLobbyView )
jsonDecGameLobbyView =
   ("gameLobbyViewGameName" := Json.Decode.string) >>= \pgameLobbyViewGameName ->
   ("gameLobbyViewPlayers" := Json.Decode.list (jsonDecPlayer)) >>= \pgameLobbyViewPlayers ->
   Json.Decode.succeed {gameLobbyViewGameName = pgameLobbyViewGameName, gameLobbyViewPlayers = pgameLobbyViewPlayers}

jsonEncGameLobbyView : GameLobbyView -> Value
jsonEncGameLobbyView  val =
   Json.Encode.object
   [ ("gameLobbyViewGameName", Json.Encode.string val.gameLobbyViewGameName)
   , ("gameLobbyViewPlayers", (Json.Encode.list << List.map jsonEncPlayer) val.gameLobbyViewPlayers)
   ]



type alias GameLobbyPreview  =
   { gameLobbyPreviewGameId: GameId
   , gameLobbyPreviewGameName: String
   , gameLobbyPreviewPlayers: (List Player)
   }

jsonDecGameLobbyPreview : Json.Decode.Decoder ( GameLobbyPreview )
jsonDecGameLobbyPreview =
   ("gameLobbyPreviewGameId" := jsonDecGameId) >>= \pgameLobbyPreviewGameId ->
   ("gameLobbyPreviewGameName" := Json.Decode.string) >>= \pgameLobbyPreviewGameName ->
   ("gameLobbyPreviewPlayers" := Json.Decode.list (jsonDecPlayer)) >>= \pgameLobbyPreviewPlayers ->
   Json.Decode.succeed {gameLobbyPreviewGameId = pgameLobbyPreviewGameId, gameLobbyPreviewGameName = pgameLobbyPreviewGameName, gameLobbyPreviewPlayers = pgameLobbyPreviewPlayers}

jsonEncGameLobbyPreview : GameLobbyPreview -> Value
jsonEncGameLobbyPreview  val =
   Json.Encode.object
   [ ("gameLobbyPreviewGameId", jsonEncGameId val.gameLobbyPreviewGameId)
   , ("gameLobbyPreviewGameName", Json.Encode.string val.gameLobbyPreviewGameName)
   , ("gameLobbyPreviewPlayers", (Json.Encode.list << List.map jsonEncPlayer) val.gameLobbyPreviewPlayers)
   ]



type alias GamePreview  =
   { gamePreviewGameId: GameId
   , gamePreviewGameName: String
   , gamePreviewPlayers: (List Player)
   }

jsonDecGamePreview : Json.Decode.Decoder ( GamePreview )
jsonDecGamePreview =
   ("gamePreviewGameId" := jsonDecGameId) >>= \pgamePreviewGameId ->
   ("gamePreviewGameName" := Json.Decode.string) >>= \pgamePreviewGameName ->
   ("gamePreviewPlayers" := Json.Decode.list (jsonDecPlayer)) >>= \pgamePreviewPlayers ->
   Json.Decode.succeed {gamePreviewGameId = pgamePreviewGameId, gamePreviewGameName = pgamePreviewGameName, gamePreviewPlayers = pgamePreviewPlayers}

jsonEncGamePreview : GamePreview -> Value
jsonEncGamePreview  val =
   Json.Encode.object
   [ ("gamePreviewGameId", jsonEncGameId val.gamePreviewGameId)
   , ("gamePreviewGameName", Json.Encode.string val.gamePreviewGameName)
   , ("gamePreviewPlayers", (Json.Encode.list << List.map jsonEncPlayer) val.gamePreviewPlayers)
   ]



type ServerError  =
    NoSuchGame GameId
    | NotInGame Player
    | ClientMsgError 
    | NotLoggedIn 
    | GameAlreadyStarted 
    | NoSuchConnection 
    | GameError_ GameError

jsonDecServerError : Json.Decode.Decoder ( ServerError )
jsonDecServerError =
    let jsonDecDictServerError = Dict.fromList
            [ ("NoSuchGame", Json.Decode.map NoSuchGame (jsonDecGameId))
            , ("NotInGame", Json.Decode.map NotInGame (jsonDecPlayer))
            , ("ClientMsgError", Json.Decode.succeed ClientMsgError)
            , ("NotLoggedIn", Json.Decode.succeed NotLoggedIn)
            , ("GameAlreadyStarted", Json.Decode.succeed GameAlreadyStarted)
            , ("NoSuchConnection", Json.Decode.succeed NoSuchConnection)
            , ("GameError_", Json.Decode.map GameError_ (jsonDecGameError))
            ]
    in  decodeSumObjectWithSingleField  "ServerError" jsonDecDictServerError

jsonEncServerError : ServerError -> Value
jsonEncServerError  val =
    let keyval v = case v of
                    NoSuchGame v1 -> ("NoSuchGame", encodeValue (jsonEncGameId v1))
                    NotInGame v1 -> ("NotInGame", encodeValue (jsonEncPlayer v1))
                    ClientMsgError  -> ("ClientMsgError", encodeValue (Json.Encode.list []))
                    NotLoggedIn  -> ("NotLoggedIn", encodeValue (Json.Encode.list []))
                    GameAlreadyStarted  -> ("GameAlreadyStarted", encodeValue (Json.Encode.list []))
                    NoSuchConnection  -> ("NoSuchConnection", encodeValue (Json.Encode.list []))
                    GameError_ v1 -> ("GameError_", encodeValue (jsonEncGameError v1))
    in encodeSumObjectWithSingleField keyval val

