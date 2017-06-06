{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WsApp (WsApp.handle) where

import           ClassyPrelude
import           ConnectionMgnt
import           GameLogic        (addAction)
import qualified GameLogic        as GL
import           Network.Protocol (CatcherGameView, GameView, RogueGameView)
import qualified Network.Protocol as Protocol
import           State
import           WsAppUtils

handle :: TVar ServerState -> Protocol.Action -> IO ()
handle serverVar action = do
    state <- readTVarIO serverVar
    let game = updateState action $ gameState state
    let catchers = withoutClient 0 (connections state)
    let maybeRogue = findConnectionById 0 state
    let rogueGameView = gameStateToRogueView game
    let catcherGameView = gameStateToCatcherView game

    case maybeRogue of
        Just rogue -> sendView rogueGameView rogue
        Nothing    -> putStrLn "There is no rogue connected"

    broadcast catcherGameView catchers

    return ()

toRealAction :: Protocol.Action -> GL.Action
toRealAction act =
    GL.OneMove
        (GL.Move
            (strToColor $ (Protocol.transportName . Protocol.transport) act)
            ((Protocol.nodeId . Protocol.node) act)
        )

updateState :: Protocol.Action -> GameState -> GameState
-- TODO: someone implement
updateState act game = game `addAction` toRealAction act

gameStateToCatcherView :: GameState -> CatcherGameView
gameStateToCatcherView state =
    let
        flattened = GL.flattenState state
    in case GL.gamestate flattened of
        Just (energies, pos, _) -> undefined
        Nothing                 -> undefined

gameStateToRogueView :: GameState -> RogueGameView
gameStateToRogueView state = undefined

strToColor :: String -> GL.Energy
strToColor "black"  = GL.BlackEnergy
strToColor "red"    = GL.Energy GL.Red
strToColor "orange" = GL.Energy GL.Orange
strToColor "blue"   = GL.Energy GL.Blue
strToColor _        = error "WsApp.strToColor: The Protocol has been broken"
-- FIXME^: server should not die
