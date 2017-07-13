{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GameNg
    ( initialState
    , updateState
    , GameState(..)
    , stateNetwork
    , statePlayerPositions
    , statePlayerEnergies
    , stateRogueHistory
    , stateNextPlayer
    ) where

import           ClassyPrelude
import           Config.InitialState
import           Config.Network      as Config
import           Data.Easy           (maybeToEither)
import           Data.Map.Strict     (insert)
import           Network.Protocol

-- TODO: is there a better insert function than Data.Map.Strict.insert?
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

-- |The game's update function.
updateState ::
       Action
    -> GameState
    -> Either GameError (GameState, RogueGameView, CatcherGameView)
updateState action state = do
    let player = actionPlayer action
    let energy = actionTransport action
    let targetNode = actionNode action
    unless (player == stateNextPlayer state) . Left $
        GameError "not player's turn"
    previousNode <-
        maybeToEither (GameError "player not found") .
        lookup player . playerPositions . statePlayerPositions $
        state
    unless (canMoveBetween (stateNetwork state) previousNode energy targetNode) .
        Left $
        GameError "Player is unable to reach this node"
    newPlayerEnergies <-
        nextPlayerEnergies (statePlayerEnergies state) player energy
    let newNextPlayer = Player $ playerId player + 1 `mod` 4 -- TODO: model all players
    let newRogueHistory =
            if playerId player == 0
                then RogueHistory $
                     (energy, Just targetNode) :
                     rogueHistory (stateRogueHistory state)
                else stateRogueHistory state
    let newPlayerPositions =
            PlayerPositions $
            insert player targetNode . playerPositions . statePlayerPositions $
            state
    let newState = state
            { statePlayerPositions = newPlayerPositions
            , statePlayerEnergies = newPlayerEnergies
            , stateRogueHistory = newRogueHistory
            , stateNextPlayer = newNextPlayer
            }
    return (newState, error "view not implemented", error "not implemented")

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
        maybeToEither (GameError "player not found") .
        lookup player . playerEnergies $
        pEnergies
    energyCount <-
        maybeToEither (GameError "energy not found") . lookup energy . energyMap $
        eMap
    unless (energyCount >= 1) . Left $ GameError "not enough energy"
    return . PlayerEnergies $
        insert
            player
            (EnergyMap . insert energy (energyCount - 1) $ energyMap eMap) .
        playerEnergies $
        pEnergies
