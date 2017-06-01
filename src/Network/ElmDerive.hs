{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ElmDerive where

import           ClassyPrelude

import           Network.Protocol

import           Data.Proxy
import           Elm.Module

elmProtocolModule :: Text
elmProtocolModule =
    pack
        $ makeElmModule "Protocol"
            [ DefineElm (Proxy :: Proxy Action)
            , DefineElm (Proxy :: Proxy Action)
            , DefineElm (Proxy :: Proxy PlayerPositions)
            , DefineElm (Proxy :: Proxy RogueGameView)
            , DefineElm (Proxy :: Proxy CatcherGameView)
            , DefineElm (Proxy :: Proxy PlayerEnergies)
            , DefineElm (Proxy :: Proxy EnergyMap)
            , DefineElm (Proxy :: Proxy Network)
            , DefineElm (Proxy :: Proxy NetworkOverlay)
            ]
