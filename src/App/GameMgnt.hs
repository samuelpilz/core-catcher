{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: remake comment
-- TODO: test

module App.GameMgnt (
    GameId,
    GameStates(..),
    HasGameStates,
    getGameStates,
    setGameStates,
    addGameState,
    removeGameState,
    findGameStateById,
    withoutGameState,
    updateGameState
    ) where

import           ClassyPrelude
import           GameState
import           Network.Protocol

-- TODO: instance MonoFoldable & MonoTraversable for GameStates
data GameStates =
    GameStates
        { gameStates :: Map GameId GameState
        , nextId     :: Int
        }

class HasGameStates state where
    getGameStates :: state -> GameStates
    setGameStates :: GameStates -> state -> state

addGameState :: HasGameStates state => TVar state -> GameState -> STM GameId
addGameState stateVar gameState = do
    state <- readTVar stateVar
    let states@GameStates{gameStates, nextId} = getGameStates state
    let newStates =
            states
                { gameStates =
                    insertMap (GameId nextId) gameState gameStates
                , nextId = 1 + nextId
                }
    writeTVar stateVar $ setGameStates newStates state
    return $ GameId nextId

-- TODO: for all STM functions, return the state after writing
updateGameState :: HasGameStates state => TVar state -> GameId -> GameState -> STM ()
updateGameState stateVar gameId gameState = do
    state <- readTVar stateVar
    let states@GameStates{gameStates} = getGameStates state
    let newStates =
            states
                { gameStates =
                    insertMap gameId gameState gameStates
                }
    writeTVar stateVar $ setGameStates newStates state


removeGameState :: HasGameStates state => TVar state -> GameId -> STM ()
removeGameState stateVar gId = do
    state <- readTVar stateVar
    let states = getGameStates state
    writeTVar stateVar $ setGameStates (withoutGameState gId states) state


-- implement HasGameStates for GameStates themselves
instance HasGameStates GameStates where
    getGameStates = id
    setGameStates = const


-- extra functions

findGameStateById :: HasGameStates state => GameId -> state -> Maybe GameState
findGameStateById gId =
    lookup gId . gameStates . getGameStates

withoutGameState :: HasGameStates state => GameId -> state -> state
withoutGameState gId state =
    let
        states@GameStates{gameStates} = getGameStates state
        newGameStates = states { gameStates = deleteMap gId gameStates }
    in
        setGameStates newGameStates state

