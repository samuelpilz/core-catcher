{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Glue
    ( GameState
    , updateState
    , initialState
    ) where

import           ClassyPrelude
import qualified Data.Map         as Map (fromList, mapKeys)
import qualified GameLogic
import qualified Lib
import qualified Network.Protocol as Protocol

type GameState = GameLogic.GameState -- export game state type

match :: Eq b => b -> [(b, a)] -> Maybe a
match b = ClassyPrelude.map snd . find ((==b) . fst)

decodeAction :: Protocol.Action -> Maybe GameLogic.Action
decodeAction act = do
    let tn = Protocol.transportName $ Protocol.transport act
    energy <- match tn [
                ("red", GameLogic.Energy GameLogic.Red),
                ("orange", GameLogic.Energy GameLogic.Orange),
                ("blue", GameLogic.Energy GameLogic.Blue),
                ("black", GameLogic.BlackEnergy)
            ]
    let vid = Protocol.nodeId $ Protocol.node act
    return $ GameLogic.OneMove $ GameLogic.Move energy vid

encodeEnergy :: GameLogic.Energy -> Protocol.Transport
encodeEnergy (GameLogic.Energy x) =
    Protocol.Transport {
        Protocol.transportName = case x of
            GameLogic.Red    -> "red"
            GameLogic.Orange -> "orange"
            GameLogic.Blue   -> "blue"
    }
encodeEnergy GameLogic.BlackEnergy =
    Protocol.Transport { Protocol.transportName = "black" }

toTupleList :: Vector a -> [(Protocol.Player, a)]
toTupleList v =
    zip
        (map (\id -> Protocol.Player { Protocol.playerId = id } ) [(0 :: Int)..])
        $ toList v

toPlayerMap :: Vector a -> Map Protocol.Player a
toPlayerMap v =
    Map.fromList $ toTupleList v

encodeEnergies :: GameLogic.PlayersEnergies -> Protocol.PlayerEnergies
encodeEnergies e =
    Protocol.PlayerEnergies {
        Protocol.playerEnergies =
            map ((\m -> Protocol.EnergyMap { Protocol.energyMap = m } ) . Map.mapKeys encodeEnergy) $ toPlayerMap e
    }

encodeNode :: GameLogic.VertexId -> Protocol.Node
encodeNode n =
    Protocol.Node {
        Protocol.nodeId = n
    }

encodePositions :: GameLogic.PlayersPos -> Protocol.PlayerPositions
encodePositions pos =
    Protocol.PlayerPositions {
        Protocol.playerPositions_ = toPlayerMap $ map encodeNode pos
    }

transportHistory :: Protocol.RogueTransportHistory --- TODO: change signature and implement
transportHistory =
    Protocol.RogueTransportHistory {
        Protocol.rogueTransportHistory = []
    }

rogueLastSeen :: Maybe Protocol.Node
rogueLastSeen = Just Protocol.Node { Protocol.nodeId = 0 }

encodeError :: Text -> Protocol.GameError
encodeError err =
     Protocol.GameError {
         Protocol.myError = err
     }

encodePlayer :: GameLogic.PlayerId -> Protocol.Player
encodePlayer p =
    Protocol.Player {
        Protocol.playerId = p
    }

updateState :: Protocol.Action -> GameLogic.GameState ->
    Either Protocol.GameError (GameLogic.GameState, Protocol.RogueGameView, Protocol.CatcherGameView)
updateState act game = do
    action <- Lib.maybeToEither (decodeAction act) (encodeError "invalid action")
    let gs = GameLogic.addAction game action
    flatgs <- Lib.mapLeft (encodeError . GameLogic.error) $ GameLogic.playersState gs
    let (pid, energies, poss, _) = flatgs
    let eposs = encodePositions poss
    let eenergies = encodeEnergies energies
    return (gs,
         Protocol.RogueView {
             Protocol.roguePlayerPositions = eposs,
             Protocol.rogueEnergies = eenergies,
             Protocol.rogueOwnHistory = transportHistory, -- TODO: implement history
             Protocol.rogueRogueLastSeen = rogueLastSeen, -- TODO: implement last seen
             Protocol.rogueNextPlayer = encodePlayer pid -- TODO: what to do with this?
         },
         Protocol.CatcherView {
             Protocol.catcherPlayerPositions = eposs, -- TODO: hide rogue
             Protocol.catcherEnergies = eenergies,
             Protocol.catcherRogueHistory = transportHistory, -- TODO: implement history
             Protocol.catcherRogueLastSeen = rogueLastSeen, -- TODO: implement last seen
             Protocol.catcherNextPlayer = encodePlayer pid
         }
        )


initialState :: GameLogic.GameState
initialState =
    GameLogic.GameState
      (fromList [1..4])
      GameLogic.samNet
      []
