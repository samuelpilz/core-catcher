{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-|

Module for connections, which can send and receive messages

-}

module App.Connection where

import           ClassyPrelude
import           Network.Protocol

class IsConnection c where
    type Pending c :: *

    -- |send a message
    sendMsg :: MonadIO m => c -> MessageForClient -> m ()

    -- |send
    recvMsg :: MonadIO m => c -> m (Maybe MessageForServer)

    sendSendableMsg :: MonadIO m => SendableToClient msg => c -> msg -> m ()
    sendSendableMsg c msg = sendMsg c $ wrapSendable msg

    multicastMsg ::
        ( SendableToClient msg
        , MonoFoldable conns
        , c ~ Element conns
        , MonadIO m
        )
        => msg -> conns -> m ()
    multicastMsg msg = omapM_ (`sendSendableMsg` msg)
