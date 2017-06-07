{-# LANGUAGE NoImplicitPrelude #-}

module Glue
    (updateState
    ) where

import           ClassyPrelude
import           GameLogic
import qualified Network.Protocol as Protocol

updateState :: Protocol.Action -> GameState ->
    Either Protocol.GameError (GameState, Protocol.RogueGameView, Protocol.CatcherGameView)
updateState act game = undefined -- game `addAction` coerce act
