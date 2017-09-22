{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module App.App (handle) where

import           App.ConnectionMgnt
import           App.State
import           ClassyPrelude       hiding (handle)
import           Config.GameConfig   (GameConfig (..))
import qualified Config.GameConfig   as Game
import           GameNg              (GameRunning (..), GameState (..),
                                      getViews)
import qualified GameNg              as Game
import           Network.Protocol

handle
    :: IsConnection conn
    => (ConnectionId, conn)
    -> TVar (ServerState conn)
    -> MessageForServer
    -> IO ()
handle (cId, client) stateVar msg = do
    putStrLn $ tshow msg
    case msg of
        Action_ action -> do
            (newState, updateResult) <- atomically $ updateGameAtomically stateVar action
            case updateResult of
                Right newGameState -> sendGameViews newGameState newState
                Left gameError -> do
                    sendSendableMsg client gameError
                    putStrLn $ "invalid action " ++ tshow gameError
        Login_ Login{ loginPlayer } -> do
            atomically $ modifyTVar stateVar
                (\state@ServerState{ playerMap } ->
                    state { playerMap = insertMap loginPlayer cId playerMap }
                )
            state <- atomically $ readTVar stateVar

            sendSendableMsg client .
                initialInfoForClient (gameState state) $
                loginPlayer

-- TODO: validation playerId matches connection-origin
-- TODO: draw transition diagrams for app


updateGameAtomically
    :: IsConnection conn
    =>  TVar (ServerState conn)
    -> Action
    -> STM (ServerState conn, Either GameError GameState)
updateGameAtomically stateVar action = do
    state <- readTVar stateVar

    let updateResult = Game.updateState action $ gameState state

    newState <- case updateResult of
        Right newGameState -> do
            let newState = state { gameState = newGameState }
            writeTVar stateVar newState
            return newState
        Left _ -> return state -- let state stay the same

    return (newState, updateResult)

sendGameViews :: IsConnection conn => GameState -> ServerState conn -> IO ()
sendGameViews (GameRunning_ game) ServerState{ stateConnections, playerMap } = do
    let views = Game.getViews game
    mapM_
        (\(player, conn) ->
            sendSendableMsg conn $
            viewForPlayer (gameRunningGameConfig game) views player
        ) .
        mapMaybe
            (\(p,cId) -> do -- maybe monad
                conn <- lookup cId $ connections stateConnections
                return (p, conn)
            ) $
        mapToList playerMap
sendGameViews (GameOver_ gameOver) ServerState{ stateConnections } =
    multicastMsg stateConnections $ Game.getGameOverView gameOver


initialInfoForClient :: GameState -> Player -> Either GameOverView InitialInfoForGame
initialInfoForClient (GameRunning_ gameRunning) player =
    Right InitialInfoForGame
        { networkForGame = network
        , initialGameView = initialView
        , initialPlayer = player
        , allPlayers = toList $ players config
        , allEnergies = [Red, Blue, Orange]
        }
    where
        config = gameRunningGameConfig gameRunning
        initialView = viewForPlayer config (getViews gameRunning) player
        network = Game.network config
initialInfoForClient (GameOver_ gameOver) _ = Left $ Game.getGameOverView gameOver

viewForPlayer :: GameConfig -> (RogueGameView, CatcherGameView) -> Player -> GameView
viewForPlayer config views player =
    (if player == head (players config) then RogueView . fst else CatcherView . snd) views
