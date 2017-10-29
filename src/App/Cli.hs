{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}


module App.Cli where

import           App.State
import           ClassyPrelude
import           EntityMgnt
import           GameState
import           Network.Protocol
import           System.Console.Haskeline

cliIO :: TVar (ServerState conn) -> ThreadId -> IO ()
cliIO stateVar serverThreadId =
    runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            cmdMay <- getInputLine "> "
            case map (words . pack) cmdMay of
                Nothing -> do
                    liftIO $ putStrLn "shutting down server"
                    liftIO $ killThread serverThreadId
                Just [] ->
                    loop
                Just (cmd:args) -> do
                    state <- liftIO $ readTVarIO stateVar
                    liftIO $ putStrLn $ cliF state cmd args
                    loop


cliF :: ServerState conn -> Text -> [Text] -> Text
cliF state "games" [] =
    case allEntities state of
        [] -> "no games"
        games -> intercalate "\n" . map prettyPrintGameSummary $ games
cliF state "game" [gameId] =
    fromMaybe ("no game with id " ++ gameId) $ do
        gId <- GameId <$> readMay gameId
        gState <- findEntityById gId state
        return $ prettyPrintGameDetails gId gState
cliF _ cmd _ =
    "command not found: " ++ cmd

prettyPrintGameSummary :: (GameId, GameState) -> Text
prettyPrintGameSummary (GameId gId, gState) =
    tshow gId ++ ": " ++ getGameName gState ++ " " ++ tshow (getGameStateEnum gState)

prettyPrintGameDetails :: GameId -> GameState -> Text
prettyPrintGameDetails (GameId gId) gState =
    "Game "  ++ tshow gId ++ ": " ++ tshow (getGameStateEnum gState) ++ "\n" ++
    "players\n" ++
    intercalate "\n"
        (map (\Player{playerName} -> "\t" ++ playerName ) $
        getGamePlayers gState)
