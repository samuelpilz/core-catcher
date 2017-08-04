{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GameNg
    ( initialState
    , getViews
    , updateState
    , GameState(..)
    ) where

import           ClassyPrelude
import           Config.GameConfig
import           Config.Network    as Config
import           Data.Easy         (maybeToEither)
import           Network.Protocol

data GameState = GameState
    { stateNetwork         :: Network
    , statePlayerPositions :: PlayerPositions
    , statePlayerEnergies  :: PlayerEnergies
    , stateRogueHistory    :: RogueHistory
    , stateNextPlayer      :: Player
    , stateGameConfig      :: GameConfig
    }

-- |The initial state of the game
initialState :: GameConfig -> GameState
initialState config =
    GameState
        Config.network
        (initialPlayerPositions config)
        (initialPlayerEnergies config)
        (RogueHistory [])
        (Player 0)
        config

-- |The game's update function.
updateState :: Action -> GameState -> Either GameError GameState

updateState
    Move { actionPlayer, actionEnergy, actionNode }
    state@GameState
        { stateNetwork
        , statePlayerPositions
        , statePlayerEnergies
        , stateRogueHistory
        , stateNextPlayer
        , stateGameConfig
        }

    = do

    unless (actionPlayer == stateNextPlayer) . Left .
        GameError $ "not player " ++ tshow (playerId actionPlayer) ++ "'s turn"

    previousNode <-
        maybeToEither (GameError "player not found") .
        lookup actionPlayer $
        statePlayerPositions

    newPlayerEnergies <-
        nextPlayerEnergies statePlayerEnergies actionPlayer actionEnergy

    unless (canMoveBetween stateNetwork previousNode actionEnergy actionNode) .
        Left .
        GameError $ "Player is unable to reach this node"

    let newNextPlayer = Player $ (playerId actionPlayer + 1) `mod` length (players stateGameConfig) -- TODO: model all players

    let newRogueHistory =
            if playerId actionPlayer == 0
                then RogueHistory $
                    (actionEnergy
                    , if
                        length stateRogueHistory `elem` rogueShowsAt stateGameConfig
                    then
                        Just actionNode else Nothing) :
                        rogueHistory stateRogueHistory
                else stateRogueHistory

    let newPlayerPositions = insertMap actionPlayer actionNode statePlayerPositions

    return state
            { statePlayerPositions = newPlayerPositions
            , statePlayerEnergies = newPlayerEnergies
            , stateRogueHistory = newRogueHistory
            , stateNextPlayer = newNextPlayer
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
        maybeToEither (GameError "player not found") . lookup player $ pEnergies
    energyCount <-
        maybeToEither (GameError "energy not found") . lookup energy $ eMap
    unless (energyCount >= 1) . Left $ GameError "not enough energy"
    return $ insertMap player (insertMap energy (energyCount - 1) eMap) pEnergies

-- |Converts the GameState into the 2 Views
getViews :: GameState -> (RogueGameView, CatcherGameView)
getViews GameState
    { statePlayerPositions
    , statePlayerEnergies
    , stateRogueHistory
    , stateNextPlayer
    } =
    ( RogueGameView
        { roguePlayerPositions = statePlayerPositions
        , rogueEnergies = statePlayerEnergies
        , rogueOwnHistory = stateRogueHistory
        , rogueNextPlayer = stateNextPlayer
        }
    , CatcherGameView
        { catcherPlayerPositions = catcherPlayerPositions
        , catcherEnergies = statePlayerEnergies
        , catcherRogueHistory = stateRogueHistory
        , catcherNextPlayer = stateNextPlayer
        }
    )
    where
        rogueShowsPosition =
            join .
            find isJust .
            map snd .
            rogueHistory $
            stateRogueHistory
        catcherPlayerPositions =
            updateMap (const rogueShowsPosition) (Player 0) statePlayerPositions


