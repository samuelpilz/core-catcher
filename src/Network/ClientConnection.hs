{-# LANGUAGE NoImplicitPrelude #-}
module Network.ClientConnection where

{-
This module manages one client connections.
-}

import           ClassyPrelude
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import           Protocol


data ClientConnection =
    Connection
        { playerId     :: Player
        , wsConnection :: WS.Connection
        }

-- TODO: functions that make sens (Hannes Plz)
sendView :: ClientConnection -> GameView -> messageSent
sendView _ _ = undefined

recvAction :: ClientConnection -> Action -> action
recvAction _ _ = undefined

{- other maybe-useful functions ... -}
