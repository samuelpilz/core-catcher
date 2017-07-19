{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.WsApp (handle, initialInfoForClient) where

import           App.Connection
import           App.ConnectionMgnt
import           App.State
import           App.WsAppUtils
import           ClassyPrelude      hiding (handle)
import           Network.Protocol
import qualified GameNg

handle :: IsConnection conn => ClientConnection conn -> TVar (ServerState conn) -> Action -> IO ()
handle client stateVar action = do
    putStrLn $ tshow action
    (newState, updateResult) <- atomically $ updateGame stateVar action

    case updateResult of
        Right (_, rogueGameView, catcherGameView) -> do

            -- send game views
            let catchers = withoutClient 0 (connections newState)
            let maybeRogue = findConnectionById 0 newState
            broadcastCatcherView catchers catcherGameView
            case maybeRogue of
                Just rogue -> sendRogueView rogue rogueGameView
                Nothing    -> putStrLn "There is no rogue connected"

        Left gameError -> do
            sendError client gameError
            putStrLn $ "invalid action " ++ tshow (myError gameError)

    return ()

updateGame ::  IsConnection conn =>  TVar (ServerState conn) -> Action
    -> STM (ServerState conn, Either GameError (GameState, RogueGameView, CatcherGameView))
updateGame stateVar action = do
    state <- readTVar stateVar
    let updateResult = GameNg.updateState action $ gameState state

    newState <- case updateResult of
        Right (newGame, _, _) -> do
            let newState = state { gameState = newGame }
            writeTVar stateVar newState
            return newState
        Left _ -> return state -- let state stay the same

    return (newState, updateResult)


initialInfoForClient :: ClientId -> InitialInfoForClient
initialInfoForClient clientId =
    InitialInfoForClient
        { initialPlayer = Player clientId
        , networkForGame = GameNg.stateNetwork GameNg.initialState
        , initialGameView = initialView
        }
    where
        initialView = (if clientId == 0 then RogueView . fst else CatcherView . snd)
            GameNg.initialViews
