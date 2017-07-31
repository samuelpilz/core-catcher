{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GameNg
    ( initialState
    , getViews
    , updateState
    , GameState(..)
    , GameRunning(..)
    , gameRunningRogueHistory
    , GameOver(..)
    , getGameOverView
    , gameNetwork
    ) where

import           ClassyPrelude
import           Config.GameConfig
import           Control.Monad.Extra (whenJust)
import           Data.Easy           (ifToMaybe, maybeToEither)
import           Network.Protocol

data GameState = GameRunning_ GameRunning | GameOver_ GameOver

data GameRunning = GameRunning
        { gameRunningGameConfig       :: GameConfig
        , gameRunningPlayerPositions  :: PlayerPositions
        , gameRunningPlayerEnergies   :: PlayerEnergies
        , gameRunningOpenRogueHistory :: OpenRogueHistory
        , gameRunningNextPlayer       :: Player
        }
    deriving (Eq, Show, Read)

-- |Function to access the shadowed version of the rogueHistory
gameRunningRogueHistory :: GameRunning -> RogueHistory
gameRunningRogueHistory = toShadowRogueHistory . gameRunningOpenRogueHistory

data GameOver =
    GameOver
        { gameOverGameConfig      :: GameConfig
        , gameOverPlayerPositions :: PlayerPositions
        , gameOverPlayerEnergies  :: PlayerEnergies
        , gameOverRogueHistory    :: OpenRogueHistory
        , gameOverWinningPlayer   :: Player
        }
    deriving (Eq, Show, Read)

-- TODO: make game-initiation better
-- |The initial state of the game
initialState :: GameConfig -> GameRunning
initialState config =
    GameRunning
        config
        (initialPlayerPositions config)
        (initialPlayerEnergies config)
        (OpenRogueHistory [])
        (Player 0)

-- |The game's update function.
updateState :: Action -> GameRunning -> Either GameError (Either GameOver GameRunning)
updateState
    Move { actionPlayer, actionEnergy, actionNode }
    state@GameRunning
        { gameRunningGameConfig
        , gameRunningPlayerPositions
        , gameRunningPlayerEnergies
        , gameRunningOpenRogueHistory
        , gameRunningNextPlayer
        }
    = do
    unless (actionPlayer == gameRunningNextPlayer) $ Left NotTurn

    previousNode <-
        maybeToEither (PlayerNotFound actionPlayer) .
        lookup actionPlayer $
        gameRunningPlayerPositions

    newPlayerEnergies <-
        nextPlayerEnergies gameRunningPlayerEnergies actionPlayer actionEnergy

    unless (canMoveBetween (network gameRunningGameConfig) previousNode actionEnergy actionNode) .
        Left $ NotReachable

    whenJust (isBlocked gameRunningPlayerPositions actionNode) $ Left . NodeBlocked

    let newNextPlayer = Player $
            (playerId actionPlayer + 1) `mod` length (players gameRunningGameConfig)

    let newRogueHistory =
            if playerId actionPlayer == 0
                then
                    (actionEnergy
                    , actionNode
                    , length gameRunningOpenRogueHistory `elem` rogueShowsAt gameRunningGameConfig
                    ) `cons`
                    gameRunningOpenRogueHistory
                else gameRunningOpenRogueHistory

    let newPlayerPositions = insertMap actionPlayer actionNode gameRunningPlayerPositions

    -- game over checking
    roguePosition <-
        maybeToEither (PlayerNotFound $ Player 0) .
        lookup (Player 0) $
        newPlayerPositions
    let rogueWonMay = do -- maybe monad
            unless (actionPlayer == Player 0) Nothing
            -- TODO: not necessary because checked implicitly
            unless (length gameRunningOpenRogueHistory == maxRounds gameRunningGameConfig) Nothing
            return $ Player 0
    let playerCaughtMay = do -- maybe monad
            when (actionPlayer == Player 0) Nothing
            -- TODO: not necessary because checked implicitly
            map fst .
                find (\(p,n) -> p /= Player 0 && n == roguePosition) .
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
                , gameRunningNextPlayer = newNextPlayer
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


isBlocked :: PlayerPositions -> Node -> Maybe Player
isBlocked pos node = map fst . find (\(p,n) -> p /= Player 0 && n == node) . mapToList $ pos

nextPlayerEnergies ::
       PlayerEnergies -> Player -> Energy -> Either GameError PlayerEnergies
nextPlayerEnergies pEnergies player energy = do
    eMap <-
        maybeToEither (PlayerNotFound player) . lookup player $ pEnergies
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
    , gameRunningOpenRogueHistory
    , gameRunningNextPlayer
    } =
    ( RogueGameView
        { roguePlayerPositions = gameRunningPlayerPositions
        , rogueEnergies = gameRunningPlayerEnergies
        , rogueOwnHistory = shadowRogueHistory
        , rogueNextPlayer = gameRunningNextPlayer
        }
    , CatcherGameView
        { catcherPlayerPositions = catcherPlayerPositions -- filtered player positions
        , catcherEnergies = gameRunningPlayerEnergies
        , catcherRogueHistory = shadowRogueHistory
        , catcherNextPlayer = gameRunningNextPlayer
        }
    )
    where
        shadowRogueHistory = toShadowRogueHistory gameRunningOpenRogueHistory
        catcherPlayerPositions =
            updateMap (const rogueShowsPosition) (Player 0) gameRunningPlayerPositions
        rogueShowsPosition =
            join .
            map snd .
            find (isJust . snd) $
            shadowRogueHistory

toShadowRogueHistory :: OpenRogueHistory -> RogueHistory
toShadowRogueHistory =
    RogueHistory .
    map (\(e,n,showing) -> (e, showing `ifToMaybe` n)) .
    openRogueHistory

-- TODO: test
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
