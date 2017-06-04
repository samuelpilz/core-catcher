{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ProtocolTest where

import           ClassyPrelude
import qualified Data.Aeson       as Aeson
import           Network.Protocol
import           Test.Framework

test_protocolDummy :: IO ()
test_protocolDummy =
    assertEqual 1 1

prop_decodeEncodeAction :: Action -> Bool
prop_decodeEncodeAction a = Just a == decodeEncode a

prop_decodeEncodePlayerPositions :: PlayerPositions -> Bool
prop_decodeEncodePlayerPositions pp = Just pp == decodeEncode pp

prop_decodeEncodePlayer :: Player -> Bool
prop_decodeEncodePlayer p = Just p == decodeEncode p

-- helper function for all deocdeEncode* tests
decodeEncode :: (Aeson.FromJSON a, Aeson.ToJSON a) => a -> Maybe a
decodeEncode = Aeson.decode . Aeson.encode
