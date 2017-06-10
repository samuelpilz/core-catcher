{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Mock.Connection where

import           App.Connection
import           ClassyPrelude

data FakeConnection = FakeConnection

--instance IsConnection FakeConnection where
--    sendText = return . const ()
