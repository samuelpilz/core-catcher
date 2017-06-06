{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WsApp where

import           ClassyPrelude
import qualified Control.Exception  as Exception
import qualified Data.Aeson         as Aeson
import           Network.Protocol   (Action, CatcherGameView, GameView,
                                     RogueGameView)
import qualified Network.WebSockets as WS


handle :: Action -> IO ()
handle _ = return ()
