{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GameNg
    ( initialState
    , initialViews
    , updateState
    , GameState(..)
    ) where

import           ClassyPrelude
import           Config.InitialState
import           Config.Network      as Config
import           Data.Easy           (maybeToEither)
import           Network.Protocol

data GameState = GameState
    { stateNetwork         :: Network
    , statePlayerPositions :: PlayerPositions
    , statePlayerEnergies  :: PlayerEnergies
    , stateRogueHistory    :: RogueHistory
    , stateNextPlayer      :: Player
    }

-- |The initial state of the game
initialState :: GameState
initialState =
    GameState
        Config.network
        initialPlayerPositions
        initialPlayerEnergies
        initialRogueHistory
        startPlayer

-- TODO: enhance setup process
initialViews :: (RogueGameView, CatcherGameView)
initialViews = getViews initialState

-- |The game's update function.
updateState ::
       Action
    -> GameState
    -> Either GameError (GameState, RogueGameView, CatcherGameView)

updateState Move { actionPlayer, actionTransport, actionNode} state = do
    unless (actionPlayer == stateNextPlayer state) . Left .
        GameError $ "not player " ++ tshow (playerId actionPlayer) ++ "'s turn"

    previousNode <-
        maybeToEither (GameError "player not found") .
        lookup actionPlayer .
        statePlayerPositions $
        state

    newPlayerEnergies <-
        nextPlayerEnergies (statePlayerEnergies state) actionPlayer actionTransport

    unless (canMoveBetween (stateNetwork state) previousNode actionTransport actionNode) .
        Left .
        GameError $ "Player is unable to reach this node"

    let newNextPlayer = Player $ (playerId actionPlayer + 1) `mod` 4 -- TODO: model all players

    let newRogueHistory =
            if playerId actionPlayer == 0
                then RogueHistory $
                     (actionTransport, Just actionNode) :
                     rogueHistory (stateRogueHistory state)
                else stateRogueHistory state

    let newPlayerPositions =
            insertMap actionPlayer actionNode
            . statePlayerPositions
            $ state

    let newState = state
            { statePlayerPositions = newPlayerPositions
            , statePlayerEnergies = newPlayerEnergies
            , stateRogueHistory = newRogueHistory
            , stateNextPlayer = newNextPlayer
            }

    let (rogueView, catcherView) = getViews newState

    return (newState, rogueView, catcherView)

canMoveBetween :: Network -> Node -> Transport -> Node -> Bool
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
       PlayerEnergies -> Player -> Transport -> Either GameError PlayerEnergies
nextPlayerEnergies pEnergies player energy = do
    eMap <-
        maybeToEither (GameError "player not found") . lookup player $ pEnergies
    energyCount <-
        maybeToEither (GameError "energy not found") . lookup energy $ eMap
    unless (energyCount >= 1) . Left $ GameError "not enough energy"
    return $ insertMap player (insertMap energy (energyCount - 1) eMap) pEnergies

getViews :: GameState -> (RogueGameView, CatcherGameView)
getViews state =
    ( RogueGameView
        { roguePlayerPositions = statePlayerPositions state
        , rogueEnergies = statePlayerEnergies state
        , rogueOwnHistory = stateRogueHistory state
        , rogueNextPlayer = stateNextPlayer state
        }
    , CatcherGameView
        { catcherPlayerPositions = statePlayerPositions state
        , catcherEnergies = statePlayerEnergies state
        , catcherRogueHistory = stateRogueHistory state
        , catcherNextPlayer = stateNextPlayer state
        }

    )


