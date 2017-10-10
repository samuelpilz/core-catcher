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

gameStatePlayers :: GameState -> [Player]
gameStatePlayers (GameLobby_ GameLobby{gameLobbyConnectedPlayers}) =
    gameLobbyConnectedPlayers
gameStatePlayers (GameRunning_ GameRunning{gameRunningGameConfig} ) =
    toList $ players gameRunningGameConfig
gameStatePlayers (GameOver_ GameOver{gameOverGameConfig} ) =
    toList $ players gameOverGameConfig

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
    , gameOverGameConfig = GameConfig { network, players, gameName }
    } =
    GameOverView
        gameOverPlayerPositions
        gameOverPlayerEnergies
        gameOverRogueHistory
        gameOverWinningPlayer
        network
        (otoList players)
        energies
        gameName
    where
        energies = keys . overlays $ network

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


-- TODO: lift for multiple players (other function as well)
initialInfoGameActive :: GameState -> Player -> Either GameOverView InitialInfoGameActive
initialInfoGameActive (GameRunning_ gameRunning) player =
    Right $ initialInfoGameActiveFromGameRunning gameRunning player
initialInfoGameActive (GameOver_ gameOver) _ = Left $ getGameOverView gameOver
initialInfoGameActive (GameLobby_ _) _ = undefined -- TODO: fix this??


initialInfoGameActiveFromGameRunning :: GameRunning -> Player -> InitialInfoGameActive
initialInfoGameActiveFromGameRunning gameRunning player =
    InitialInfoGameActive
        { networkForGame = gameNetwork
        , initialGameView = initialView
        , startingPlayer = head $ players config
        , initialInfoAllPlayers = toList $ players config
        , initialInfoAllEnergies = [Red, Blue, Orange]
        , initialInfoGameName = gameName config
        }
    where
        config = gameRunningGameConfig gameRunning
        initialView = viewForPlayer config (getViews gameRunning) player
        gameNetwork = network config


-- TODO: ADT for preview
previewInfo :: GameId -> GameState -> Maybe (Either GameLobbyPreview GamePreview)
previewInfo gameId (GameLobby_ GameLobby{gameLobbyGameName, gameLobbyConnectedPlayers}) =
    Just . Left $ GameLobbyPreview gameId gameLobbyGameName gameLobbyConnectedPlayers
previewInfo gameId (GameRunning_ GameRunning {gameRunningGameConfig = GameConfig{players, gameName}}) =
    Just . Right . GamePreview gameId gameName $ toList players
previewInfo _ (GameOver_ _) = Nothing


allPreviewInfos :: [(GameId, GameState)] -> ([GameLobbyPreview], [GamePreview])
allPreviewInfos [] = ([], [])
allPreviewInfos ((gId,s):gs) =
    case previewInfo gId s of
        Just (Left lobby) -> (lobby:lobbies, previews)
        Just (Right preview) -> (lobbies, preview:previews)
        Nothing -> (lobbies, previews)
    where
        (lobbies, previews) = allPreviewInfos gs

getGameLobby :: GameState -> Maybe GameLobby
getGameLobby (GameLobby_ l) = Just l
getGameLobby _ = Nothing


lobbyAddPlayer :: Player -> GameLobby -> GameLobby
lobbyAddPlayer p l@GameLobby{gameLobbyConnectedPlayers} =
    l { gameLobbyConnectedPlayers = gameLobbyConnectedPlayers ++ [p] }
