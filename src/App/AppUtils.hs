{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.AppUtils where

import           App.ConnectionMgnt
import           App.GameMgnt
import           App.State
import           ClassyPrelude
import           Control.Error.Util         ((??))
import           Control.Monad.Trans.Except
import           Network.Protocol


getGameIdFromConnection :: Monad m => ConnectionState -> ExceptT ServerError m GameId
getGameIdFromConnection connState =
    connectionInGame connState ??
        (maybe NotLoggedIn NotInGame $
        connectionLoggedInPlayer connState)

-- case connectionInGame connState of
--     Nothing -> case connectionLoggedInPlayer connState of
--         Nothing ->
--             throwE NotLoggedIn
--         Just player ->
--             throwE $ NotInGame player
--     Just gameId -> return gameId

msgForOne :: ConnectionId -> MessageForClient -> [(ConnectionId, MessageForClient)]
msgForOne = singletonMap

