{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Glue
    (updateState
    ) where

import           ClassyPrelude
import           Data.Map         as Map
import qualified GameLogic
import qualified Lib
import qualified Network.Protocol as Protocol

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

encodeEnergies :: GameLogic.PlayersEnergies -> Protocol.PlayerEnergies
encodeEnergies e =
    Protocol.PlayerEnergies {
        Protocol.playerEnergies =
            Map.fromList (zip
                (ClassyPrelude.map (\id -> Protocol.Player { Protocol.playerId = id } ) [(0 :: Int)..])
                (ClassyPrelude.map ((\m -> Protocol.EnergyMap { Protocol.energyMap = m } ) . mapKeys encodeEnergy) $ ClassyPrelude.toList e)) }

{-
updateState :: Protocol.Action -> GameState ->
    Either Protocol.GameError (GameState, Protocol.RogueGameView, Protocol.CatcherGameView)
updateState act game = do
    let gs = addAction game $ decodeAction act
    flatgs <- Lib.mapLeft (\e -> Protocol.GameError { myError = e }) $ playersState gs
    let (pid, energies, poss, _) = flatgs
    return (gs, )

-}
updateState :: Protocol.Action -> GameLogic.GameState ->
    Either Protocol.GameError (GameLogic.GameState, Protocol.RogueGameView, Protocol.CatcherGameView)
updateState = undefined
