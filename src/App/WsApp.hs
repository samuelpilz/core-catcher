{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.WsApp (handle, initialInfoForClient) where

import           App.Connection
import           App.ConnectionMgnt
import           App.State
import           App.WsAppUtils
import           ClassyPrelude      hiding (handle)
import qualified Config.GameConfig  as Game
import           Data.Easy
import qualified GameNg             as Game
import           Network.Protocol

handle :: IsConnection conn => ClientConnection conn -> TVar (ServerState conn) -> Action -> IO ()
handle client stateVar action = do
    putStrLn $ tshow action
    (newState, errMay) <- atomically $ updateGame stateVar action

    case errMay of
        Nothing -> do
            let (rogueGameView, catcherGameView) = Game.getViews $ gameState newState
            -- send game views
            let catchers = withoutClient 0 (connections newState)
            let maybeRogue = findConnectionById 0 newState
            broadcastCatcherView catchers catcherGameView
            case maybeRogue of
                Just rogue -> sendRogueView rogue rogueGameView
                Nothing    -> putStrLn "There is no rogue connected"

        Just gameError -> do
            sendError client gameError
            putStrLn $ "invalid action " ++ tshow (myError gameError)

    return ()

updateGame ::  IsConnection conn =>  TVar (ServerState conn) -> Action
    -> STM (ServerState conn, Maybe GameError)
updateGame stateVar action = do
    state <- readTVar stateVar
    let updateResult = Game.updateState action $ gameState state

    newState <- case updateResult of
        Right newGame -> do
            let newState = state { gameState = newGame }
            writeTVar stateVar newState
            return newState
        Left _ -> return state

    return (newState, leftToMaybe updateResult)


initialInfoForClient :: Game.GameConfig -> ClientId -> InitialInfoForClient
initialInfoForClient config clientId =
    InitialInfoForClient
        { initialPlayer = Player clientId
        , networkForGame = network
        , initialGameView = initialView
        }
    where
        initialState = Game.initialState config
        initialView = (if clientId == 0 then RogueView . fst else CatcherView . snd)
            (Game.getViews initialState)
        network = Game.stateNetwork initialState

