{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Mock.Connection where

import ClassyPrelude
import App.Connection

data FakeConnection = FakeConnection

instance IsConnection FakeConnection where
    sendText = return . const ()
