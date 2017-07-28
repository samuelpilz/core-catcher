{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GameNg
    ( initialState
    , getViews
    , updateState
    , GameState(..)
    , GameRunning(..)
    , GameOver(..)
    , getGameOverView
    , gameNetwork
    ) where

import           ClassyPrelude
import           Config.GameConfig
import           Data.Easy         (maybeToEither)
import           Network.Protocol

data GameState = GameRunning_ GameRunning | GameOver_ GameOver

data GameRunning = GameRunning
    { gameRunningGameConfig      :: GameConfig
    , gameRunningPlayerPositions :: PlayerPositions
    , gameRunningPlayerEnergies  :: PlayerEnergies
    , gameRunningRogueHistory    :: RogueHistory
    , gameRunningNextPlayer      :: Player
    }

data GameOver = GameOver
    { gameOverGameConfig      :: GameConfig
    , gameOverPlayerPositions :: PlayerPositions
    , gameOverPlayerEnergies  :: PlayerEnergies
    , gameOverRogueHistory    :: OpenRogueHistory
    }

-- TODO: make game-initiation better
-- |The initial state of the game
initialState :: GameConfig -> GameRunning
initialState config =
    GameRunning
        config
        (initialPlayerPositions config)
        (initialPlayerEnergies config)
        (RogueHistory [])
        (Player 0)

-- |The game's update function.
updateState :: Action -> GameRunning -> Either GameError (Either GameOver GameRunning)
updateState
    Move { actionPlayer, actionEnergy, actionNode }
    state@GameRunning
        { gameRunningGameConfig
        , gameRunningPlayerPositions
        , gameRunningPlayerEnergies
        , gameRunningRogueHistory
        , gameRunningNextPlayer
        }
    = do
    unless (actionPlayer == gameRunningNextPlayer) $ Left NotTurn

    previousNode <-
        maybeToEither PlayerNotFound .
        lookup actionPlayer $
        gameRunningPlayerPositions

    newPlayerEnergies <-
        nextPlayerEnergies gameRunningPlayerEnergies actionPlayer actionEnergy

    unless (canMoveBetween (network gameRunningGameConfig) previousNode actionEnergy actionNode) .
        Left $ NotReachable

    let newNextPlayer = Player $
            (playerId actionPlayer + 1) `mod` length (players gameRunningGameConfig)

    let newRogueHistory =
            if playerId actionPlayer == 0
                then RogueHistory $
                    (actionEnergy
                    , if
                        length gameRunningRogueHistory `elem` rogueShowsAt gameRunningGameConfig
                    then
                        Just actionNode else Nothing) :
                        rogueHistory gameRunningRogueHistory
                else gameRunningRogueHistory

    let newPlayerPositions = insertMap actionPlayer actionNode gameRunningPlayerPositions

    -- TODO: check if player collisions & check if game over
    return . Right $ state
            { gameRunningPlayerPositions = newPlayerPositions
            , gameRunningPlayerEnergies = newPlayerEnergies
            , gameRunningRogueHistory = newRogueHistory
            , gameRunningNextPlayer = newNextPlayer
            }

canMoveBetween :: Network -> Node -> Energy -> Node -> Bool
canMoveBetween net from energy to =
    isJust $ -- true, if the do bock returns Just ()
    do
        overlay <- lookup energy . overlays $ net
        let edges = overlayEdges overlay
        unless -- returns Just () if such pair is found
            (any
                 (\(n1, n2) ->
                      (n1 == from && n2 == to) || (n1 == to && n2 == from)) .
             map edge $
             edges)
            Nothing

nextPlayerEnergies ::
       PlayerEnergies -> Player -> Energy -> Either GameError PlayerEnergies
nextPlayerEnergies pEnergies player energy = do
    eMap <-
        maybeToEither PlayerNotFound . lookup player $ pEnergies
    energyCount <-
        maybeToEither EnergyNotFound . lookup energy $ eMap
    unless (energyCount >= 1) . Left $ NotEnoughEnergy
    return $ insertMap player (insertMap energy (energyCount - 1) eMap) pEnergies

-- |Converts the GameState into the 2 Views
getViews :: GameRunning -> (RogueGameView, CatcherGameView)
getViews
    GameRunning
    { gameRunningPlayerPositions
    , gameRunningPlayerEnergies
    , gameRunningRogueHistory
    , gameRunningNextPlayer
    } =
    ( RogueGameView
        { roguePlayerPositions = gameRunningPlayerPositions
        , rogueEnergies = gameRunningPlayerEnergies
        , rogueOwnHistory = gameRunningRogueHistory
        , rogueNextPlayer = gameRunningNextPlayer
        }
    , CatcherGameView
        { catcherPlayerPositions = catcherPlayerPositions -- updated player positions
        , catcherEnergies = gameRunningPlayerEnergies
        , catcherRogueHistory = gameRunningRogueHistory
        , catcherNextPlayer = gameRunningNextPlayer
        }
    )
    where
        rogueShowsPosition =
            join .
            find isJust .
            map snd .
            rogueHistory $
            gameRunningRogueHistory
        catcherPlayerPositions =
            updateMap (const rogueShowsPosition) (Player 0) gameRunningPlayerPositions

getGameOverView :: GameOver -> GameOverView
getGameOverView GameOver
    { gameOverPlayerPositions
    , gameOverPlayerEnergies
    , gameOverRogueHistory
    } =
    GameOverView
        gameOverPlayerPositions
        gameOverPlayerEnergies
        gameOverRogueHistory

gameNetwork :: GameState -> Network
gameNetwork = network . gameConfig

gameConfig :: GameState -> GameConfig
gameConfig (GameRunning_ gameRunning) = gameRunningGameConfig gameRunning
gameConfig (GameOver_ gameOver)       = gameOverGameConfig gameOver
