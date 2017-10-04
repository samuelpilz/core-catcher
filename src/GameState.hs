{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module GameState where

import           ClassyPrelude
import           Config.GameConfig
import           Data.Easy         (ifToMaybe)
import           Data.List         (cycle)
import           Network.Protocol

data GameState = GameLobby_ GameLobby | GameRunning_ GameRunning | GameOver_ GameOver

data GameLobby =
    GameLobby
        { gameLobbyGameName         :: Text
        , gameLobbyConnectedPlayers :: [Player]
        }
    deriving (Eq, Show, Read)

data GameRunning =
    GameRunning
        { gameRunningGameConfig       :: GameConfig
        , gameRunningPlayerPositions  :: PlayerPositions
        , gameRunningPlayerEnergies   :: PlayerEnergies
        , gameRunningOpenRogueHistory :: OpenRogueHistory
        , gameRunningNextPlayers      :: [Player]
        }
    deriving (Eq, Show, Read)

-- |Function to access the shadowed version of the rogueHistory
gameRunningRogueHistory :: GameRunning -> ShadowRogueHistory
gameRunningRogueHistory = toShadowRogueHistory . gameRunningOpenRogueHistory

data GameOver =
    GameOver
        { gameOverGameConfig      :: GameConfig
        , gameOverPlayerPositions :: PlayerPositions
        , gameOverPlayerEnergies  :: PlayerEnergies
        , gameOverRogueHistory    :: OpenRogueHistory
        , gameOverWinningPlayer   :: Player
        }
    deriving (Eq, Show, Read)

-- |The initial state of the game
initialStateFromConfig :: GameConfig -> GameRunning
initialStateFromConfig config =
    GameRunning
        config
        (initialPlayerPositions config)
        (initialPlayerEnergies config)
        (OpenRogueHistory [])
        (cycle . toList . players $ config)

-- |Converts the GameState into the 2 Views
getViews :: GameRunning -> (RogueGameView, CatcherGameView)
getViews
    GameRunning
    { gameRunningGameConfig = GameConfig {players}
    , gameRunningPlayerPositions
    , gameRunningPlayerEnergies
    , gameRunningOpenRogueHistory
    , gameRunningNextPlayers
    } =
    ( RogueGameView
        { roguePlayerPositions = gameRunningPlayerPositions
        , rogueEnergies = gameRunningPlayerEnergies
        , rogueOwnHistory = shadowRogueHistory
        , rogueNextPlayer = nextPlayer
        }
    , CatcherGameView
        { catcherPlayerPositions = catcherPlayerPositions -- filtered player positions
        , catcherEnergies = gameRunningPlayerEnergies
        , catcherRogueHistory = shadowRogueHistory
        , catcherNextPlayer = nextPlayer
        }
    )
    where
        roguePlayer = head players
        nextPlayer = headEx gameRunningNextPlayers

        shadowRogueHistory = toShadowRogueHistory gameRunningOpenRogueHistory
        catcherPlayerPositions =
            updateMap (const rogueShowsPosition) roguePlayer gameRunningPlayerPositions
        rogueShowsPosition =
            join .
            map snd .
            find (isJust . snd) $
            shadowRogueHistory

toShadowRogueHistory :: OpenRogueHistory -> ShadowRogueHistory
toShadowRogueHistory =
    ShadowRogueHistory .
    map (\(e,n,showing) -> (e, showing `ifToMaybe` n)) .
    openRogueHistory

getGameOverView :: GameOver -> GameOverView
getGameOverView GameOver
    { gameOverPlayerPositions
    , gameOverPlayerEnergies
    , gameOverRogueHistory
    , gameOverWinningPlayer
    , gameOverGameConfig = GameConfig { network }
    } =
    GameOverView
        gameOverPlayerPositions
        gameOverPlayerEnergies
        gameOverRogueHistory
        gameOverWinningPlayer
        network

getGameLobbyView :: GameLobby -> GameLobbyView
getGameLobbyView GameLobby
    { gameLobbyGameName
    , gameLobbyConnectedPlayers
    } =
    GameLobbyView
        gameLobbyGameName
        gameLobbyConnectedPlayers


viewForGameState :: GameState -> Player -> MessageForClient
viewForGameState (GameLobby_ lobby) _ = GameLobbyView_ $ getGameLobbyView lobby
viewForGameState (GameRunning_ gameRunning) player =
    GameView_ $ viewForPlayer (gameRunningGameConfig gameRunning) (getViews gameRunning) player
viewForGameState (GameOver_ gameOver) _ = GameOverView_ $ getGameOverView gameOver


-- |helper function for selecting the correct view for a given player
viewForPlayer :: GameConfig -> (RogueGameView, CatcherGameView) -> Player -> GameView
viewForPlayer config views player =
    (if player == head (players config) then RogueView . fst else CatcherView . snd) views


initialInfoGameActive :: GameState -> Player -> Either GameOverView InitialInfoGameActive
initialInfoGameActive (GameRunning_ gameRunning) player =
    Right InitialInfoGameActive
        { networkForGame = initialNetwork
        , initialGameView = initialView
        , initialPlayer = player
        , allPlayers = toList $ players config
        , allEnergies = [Red, Blue, Orange]
        }
    where
        config = gameRunningGameConfig gameRunning
        initialView = viewForPlayer config (getViews gameRunning) player
        initialNetwork = network config
initialInfoGameActive (GameOver_ gameOver) _ = Left $ getGameOverView gameOver
initialInfoGameActive (GameLobby_ _) _ = undefined -- TODO: fix this??
