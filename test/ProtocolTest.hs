{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ProtocolTest where

import           ClassyPrelude
import           GameLogic      ((>>|))
import           Lib            (mapRight)
import           Test.Framework

test_protocolDummy :: IO ()
test_protocolDummy =
    assertEqual 1 1
