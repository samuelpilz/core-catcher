{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module MainTest where

import           ClassyPrelude
import qualified Data.Aeson                    as Aeson

import           Network.Protocol
import           Test.Framework

import qualified Network.WebSockets            as WS
import           Network.WebSockets.Connection
