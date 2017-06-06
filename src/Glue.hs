{-# LANGUAGE NoImplicitPrelude #-}

module Glue
    ( gameStateToCatcherView
    , gameStateToRogueView
    , updateState
    ) where

import           ClassyPrelude
import           GameLogic
import qualified Network.Protocol as Protocol

gameStateToCatcherView :: GameState -> Protocol.CatcherGameView
gameStateToCatcherView = undefined

gameStateToRogueView :: GameState -> Protocol.RogueGameView
gameStateToRogueView = undefined

coerce :: Protocol.Action -> Action
coerce = undefined

updateState :: Protocol.Action -> GameState -> GameState
updateState act game = game `addAction` coerce act
