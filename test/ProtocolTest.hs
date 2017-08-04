{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ProtocolTest where

import           ClassyPrelude
import qualified Data.Aeson       as Aeson
import           Network.Protocol
import           Test.Framework

test_protocolDummy :: IO ()
test_protocolDummy = return ()

prop_decodeEncodeAction :: Action -> Bool
prop_decodeEncodeAction a = Just a == decodeEncode a

prop_decodeEncodePlayerPositions :: PlayerPositions -> Bool
prop_decodeEncodePlayerPositions p = Just p == decodeEncode p

prop_decodeEncodePlayer :: Player -> Bool
prop_decodeEncodePlayer p = Just p == decodeEncode p

prop_decodeEncodeEdge :: Edge -> Bool
prop_decodeEncodeEdge a = Just a == decodeEncode a

prop_decodeEncodeEnergy :: Energy -> Bool
prop_decodeEncodeEnergy p = Just p == decodeEncode p

prop_decodeEncodeEnergyMap :: EnergyMap -> Bool
prop_decodeEncodeEnergyMap p = Just p == decodeEncode p

prop_decodeEncodePlayerEnergies :: PlayerEnergies -> Bool
prop_decodeEncodePlayerEnergies p = Just p == decodeEncode p

prop_decodeEncodeRogueHistory :: RogueHistory -> Bool
prop_decodeEncodeRogueHistory p = Just p == decodeEncode p

prop_decodeEncodeRogueGameView :: RogueGameView -> Bool
prop_decodeEncodeRogueGameView p = Just p == decodeEncode p

prop_decodeEncodeCatcherGameView :: CatcherGameView -> Bool
prop_decodeEncodeCatcherGameView p = Just p == decodeEncode p

prop_decodeEncodeNetwork :: Network -> Bool
prop_decodeEncodeNetwork p = Just p == decodeEncode p

prop_decodeEncodeNetworkOverlay :: NetworkOverlay -> Bool
prop_decodeEncodeNetworkOverlay p = Just p == decodeEncode p

-- helper function for all deocdeEncode* tests
decodeEncode :: (Aeson.FromJSON a, Aeson.ToJSON a) => a -> Maybe a
decodeEncode = Aeson.decode . Aeson.encode
