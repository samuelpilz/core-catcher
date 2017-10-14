{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.AppUtils where

import           App.ConnectionState
import           App.State
import           ClassyPrelude
import           Control.Error.Util         ((??))
import           Control.Monad.Trans.Except
import           Network.Protocol

-- TODO: refine these functions
getGameIdFromConnection :: Monad m => ConnectionState -> ExceptT ServerError m GameId
getGameIdFromConnection connState =
    connectionInGame connState ??
        maybe NotLoggedIn NotInGame (connectionLoggedInPlayer connState)

msgForOne :: ConnectionId -> MessageForClient -> [(ConnectionId, MessageForClient)]
msgForOne = singletonMap

