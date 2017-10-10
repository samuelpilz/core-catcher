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
            , DefineElm (Proxy :: Proxy PlayerPositions)
            , DefineElm (Proxy :: Proxy RogueGameView)
            , DefineElm (Proxy :: Proxy CatcherGameView)
            , DefineElm (Proxy :: Proxy GameView)
            , DefineElm (Proxy :: Proxy PlayerEnergies)
            , DefineElm (Proxy :: Proxy EnergyMap)
            , DefineElm (Proxy :: Proxy Network)
            , DefineElm (Proxy :: Proxy NetworkOverlay)
            , DefineElm (Proxy :: Proxy Player)
            , DefineElm (Proxy :: Proxy GameId)
            , DefineElm (Proxy :: Proxy Edge)
            , DefineElm (Proxy :: Proxy Node)
            , DefineElm (Proxy :: Proxy Energy)
            , DefineElm (Proxy :: Proxy ShadowRogueHistory)
            , DefineElm (Proxy :: Proxy OpenRogueHistory)
            , DefineElm (Proxy :: Proxy RogueHistory)
            , DefineElm (Proxy :: Proxy GameError)
            , DefineElm (Proxy :: Proxy GameOverView)
            , DefineElm (Proxy :: Proxy InitialInfoGameActive)
            , DefineElm (Proxy :: Proxy MessageForServer)
            , DefineElm (Proxy :: Proxy MessageForClient)
            , DefineElm (Proxy :: Proxy Login)
            , DefineElm (Proxy :: Proxy JoinGame)
            , DefineElm (Proxy :: Proxy LoginFail)
            , DefineElm (Proxy :: Proxy PlayerHome)
            , DefineElm (Proxy :: Proxy CreateNewGame)
            , DefineElm (Proxy :: Proxy GameLobbyView)
            , DefineElm (Proxy :: Proxy GameLobbyPreview)
            , DefineElm (Proxy :: Proxy GamePreview)
            , DefineElm (Proxy :: Proxy ServerError)
            ]
