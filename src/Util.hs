{-# LANGUAGE NoImplicitPrelude #-}
module Util (defaultGame) where

import           ClassyPrelude
import           GameLogic

defaultGame :: GameState
defaultGame =
    GameState
      (fromList [1..4])
      someNet
      []
