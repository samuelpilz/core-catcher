{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GameNg
    ( updateState
    , actionForGameRunning
    ) where

import           ClassyPrelude
import           Config.GameConfig
import           Control.Monad.Extra (whenJust)
import           Data.Easy           (ifToMaybe, maybeToEither)
import           Data.List           (cycle)
import           GameState
import           Network.Protocol

-- |Update the state with an action. returns the error GameIsOver if the state is in game-over state
updateState :: Action -> GameState -> Either GameError GameState
updateState _ (GameOver_ _) = Left GameIsOver
updateState action (GameRunning_ gameRunning) =
    map (either GameOver_ GameRunning_) $ actionForGameRunning action gameRunning

-- |Add an action for the running game.
actionForGameRunning :: Action -> GameRunning -> Either GameError (Either GameOver GameRunning)
actionForGameRunning
    Move { actionPlayer, actionEnergy, actionNode }
    state@GameRunning
        { gameRunningGameConfig =
            gameRunningGameConfig@GameConfig
                { network
                , rogueShowsAt
                , maxRounds
                }
        , gameRunningPlayerPositions
        , gameRunningPlayerEnergies
        , gameRunningOpenRogueHistory
        , gameRunningNextPlayers
        }
    = do
    let roguePlayer = getRogue gameRunningGameConfig
    let nextPlayer = headEx gameRunningNextPlayers
    let otherNextPlayers = tailEx gameRunningNextPlayers

    unless (actionPlayer == nextPlayer) . Left $ NotTurn nextPlayer

    previousNode <-
        maybeToEither (PlayerNotFound actionPlayer) .
        lookup actionPlayer $
        gameRunningPlayerPositions

    unless (canMoveBetween network previousNode actionEnergy actionNode) .
        Left $ NotReachable previousNode actionEnergy actionNode

    newPlayerEnergies <-
        nextPlayerEnergies gameRunningPlayerEnergies actionPlayer actionEnergy


    whenJust
        (isBlocked gameRunningPlayerPositions roguePlayer actionNode)
        $ Left . NodeBlocked

    let newNextPlayers = otherNextPlayers -- TODO: implement skipping

    let newRogueHistory =
            if actionPlayer == roguePlayer
                then
                    (actionEnergy
                    , actionNode
                    , length gameRunningOpenRogueHistory `elem` rogueShowsAt
                    ) `cons`
                    gameRunningOpenRogueHistory
                else gameRunningOpenRogueHistory

    let newPlayerPositions = insertMap actionPlayer actionNode gameRunningPlayerPositions

    -- game over checking
    roguePosition <-
        maybeToEither (PlayerNotFound roguePlayer) .
        lookup roguePlayer $
        newPlayerPositions
    let rogueWonMay = do -- maybe monad
            unless (actionPlayer == roguePlayer) Nothing
            -- check not necessary because checked implicitly above, but here for clarity
            unless (length gameRunningOpenRogueHistory == maxRounds) Nothing
            return roguePlayer
    let playerCaughtMay = do -- maybe monad
            when (actionPlayer == roguePlayer) Nothing
            -- check not necessary because checked implicitly above, but here for clarity
            map fst .
                find (\(p,n) -> p /= roguePlayer && n == roguePosition) .
                mapToList $
                newPlayerPositions
    let winningPlayerMay = rogueWonMay <|> playerCaughtMay

    return $ case winningPlayerMay of
        Just winningPlayer ->
            Left GameOver
                 { gameOverGameConfig = gameRunningGameConfig
                 , gameOverPlayerEnergies = newPlayerEnergies
                 , gameOverPlayerPositions = newPlayerPositions
                 , gameOverRogueHistory = gameRunningOpenRogueHistory
                 , gameOverWinningPlayer = winningPlayer
                 }
        Nothing ->
            Right state
                { gameRunningPlayerPositions = newPlayerPositions
                , gameRunningPlayerEnergies = newPlayerEnergies
                , gameRunningOpenRogueHistory = newRogueHistory
                , gameRunningNextPlayers = newNextPlayers
                }

canMoveBetween :: Network -> Node -> Energy -> Node -> Bool
canMoveBetween net from energy to =
    isJust $ -- true, if the do bock returns Just ()
    do -- maybe monad
        overlay <- lookup energy . overlays $ net
        let edges = overlayEdges overlay
        unless -- returns Just () if such pair is found
            (any
                 (\(n1, n2) ->
                      (n1 == from && n2 == to) || (n1 == to && n2 == from)) .
             map edge $
             edges)
            Nothing


isBlocked :: PlayerPositions -> Player -> Node -> Maybe Player
isBlocked pos roguePlayer node =
    map fst .
    find (\(p,n) -> p /= roguePlayer && n == node) .
    mapToList $
    pos

nextPlayerEnergies ::
       PlayerEnergies -> Player -> Energy -> Either GameError PlayerEnergies
nextPlayerEnergies pEnergies player energy = do
    eMap <-
        maybeToEither (PlayerNotFound player) . lookup player $ pEnergies
    energyCount <-
        maybeToEither (EnergyNotFound energy) . lookup energy $ eMap
    unless (energyCount >= 1) . Left $ NotEnoughEnergy
    return $ insertMap player (insertMap energy (energyCount - 1) eMap) pEnergies
