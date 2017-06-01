{-# LANGUAGE NoImplicitPrelude #-}
module Network.ClientConnection where

{-
This module manages one client connections.
-}

import           ClassyPrelude
import           Network.Protocol
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS


data ClientConnection =
    Connection
        { playerId     :: Player
        , wsConnection :: WS.Connection
        }

-- TODO: functions that make sense (Hannes plz)
sendView :: GameView view => ClientConnection -> view -> messageSent
sendView _ _ = undefined

recvAction :: GameView view => ClientConnection -> view -> action
recvAction _ _ = undefined

broadcast :: GameView view => [ClientConnection] -> view  -> messageSent
broadcast _ _ = undefined
{- other maybe-useful functions ... -}
