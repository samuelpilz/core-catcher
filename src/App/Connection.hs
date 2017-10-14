{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- TODO: comment
module App.Connection where

import           ClassyPrelude
import           Network.Protocol

class IsConnection c where
    type Pending c :: *

    sendMsg :: c -> MessageForClient -> IO ()

    recvMsg :: c -> IO (Maybe MessageForServer)

    sendSendableMsg :: SendableToClient msg => c -> msg -> IO ()
    sendSendableMsg c msg = sendMsg c $ wrapSendable msg

-- TODO: necessary??
    acceptRequest ::  Pending c -> IO c

    multicastMsg ::
        (SendableToClient msg, MonoFoldable conns, c ~ Element conns)
        => msg -> conns -> IO ()
    multicastMsg msg = omapM_ (`sendSendableMsg` msg)
