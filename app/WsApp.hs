{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
This module implements the logic of the WebSocket App
-}

module WsApp where

import           ClassyPrelude
import qualified Control.Exception  as Exception
--import           Network.Protocol
import           ConnectionMgnt
import qualified Network.WebSockets as WS
