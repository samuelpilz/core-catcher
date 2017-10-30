{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module GameNg
    ( updateState
    , actionForGameRunning
    , startGame
    ) where

import           ClassyPrelude
import           Config.GameConfig
import           Control.Error.Util        ((??))
import           Control.Monad.Error.Class
import           Control.Monad.Extra       (whenJust)
import           GameState
import           Network.Protocol
import           System.Random             (RandomGen)

-- TODO: use Except instead of Either

-- |Update the state with an action. returns the error GameIsOver if the state is in game-over state
updateState ::
    ( Monad m
    , MonadError m
    , ErrorType m ~ GameError
    )
    => Action
    -> GameState
    -> m GameState
updateState _ (GameLobby_ _) = throwError GameNotStarted
updateState _ (GameOver_ _) = throwError GameIsOver
updateState action (GameRunning_ gameRunning) =
    map (either GameOver_ GameRunning_) $ actionForGameRunning action gameRunning

startGame ::
    ( Monad m
    , MonadError m
    , ErrorType m ~ GameError
    , RandomGen gen
    )
    => gen
    -> GameLobby
    -> m GameRunning
startGame gen GameLobby{ gameLobbyGameName, gameLobbyConnectedPlayers } = do
    players <- fromNullable (fromList gameLobbyConnectedPlayers) ?? NoPlayersConnected
    return . initialStateFromConfig $ defaultConfigForPlayers gen gameLobbyGameName players

-- |Add an action for the running game.
actionForGameRunning ::
    ( Monad m
    , MonadError m
    , ErrorType m ~ GameError
    )
    => Action
    -> GameRunning
    -> m (Either GameOver GameRunning)
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

    unless (actionPlayer == nextPlayer) $ throwError $ NotTurn nextPlayer

    previousNode <- lookup actionPlayer gameRunningPlayerPositions
            ?? PlayerNotFound actionPlayer

    unless (canMoveBetween network previousNode actionEnergy actionNode) .
        throwError $ NotReachable previousNode actionEnergy actionNode

    newPlayerEnergies <-
        nextPlayerEnergies gameRunningPlayerEnergies actionPlayer actionEnergy


    whenJust
        (isBlocked gameRunningPlayerPositions roguePlayer actionNode)
        $ throwError . NodeBlocked

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
        lookup roguePlayer newPlayerPositions ?? PlayerNotFound roguePlayer

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
    ( Monad m
    , MonadError m
    , ErrorType m ~ GameError
    )
    => PlayerEnergies
    -> Player
    -> Energy
    -> m PlayerEnergies
nextPlayerEnergies pEnergies player energy = do
    eMap <-
        lookup player pEnergies ?? PlayerNotFound player
    energyCount <-
        lookup energy eMap ?? EnergyNotFound energy
    unless (energyCount >= 1) $ throwError NotEnoughEnergy
    return $ insertMap player (insertMap energy (energyCount - 1) eMap) pEnergies
