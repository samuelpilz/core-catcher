{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ElmDerive where

import           ClassyPrelude
import           Data.ByteString.Lazy

import           Network.Protocol

import           Data.Proxy
import           Elm.Module

ma :: String
ma =
    makeElmModule "Foo"
        [ DefineElm (Proxy :: Proxy Action)
        ]
