module Experimental.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Protocol exposing (..)
import ProtocolEmpty exposing (..)
import Experimental.ClientState exposing (..)
import AllDict exposing (..)
import EveryDict
import Experimental.MsgButtons exposing (..)
import Experimental.DebugView exposing (debugView)
import View.MapView exposing (..)
import View.EnergyView exposing (..)


view : ClientModel -> Html Msg
view state =
    div []
        (div [ style [ ( "height", "600px" ) ] ] (appView state)
            :: [ hr [] []
               , debugView state
               ]
        )


appView : ClientModel -> List (Html Msg)
appView state =
    case state.state of
        LandingArea_ landingAreaState landingArea ->
            landingAreaView state ( landingAreaState, landingArea )

        LoggedIn_ loggedInState loggedIn ->
            loggedInView state ( loggedInState, loggedIn )

        InGame_ inGameState inGame ->
            inGameView state ( inGameState, inGame )

        Disconnected disconnected ->
            [ text "Disconnected" ]

        Reconnected reconnected ->
            [ text "Reconnected" ]


landingAreaView : ClientModel -> ( LandingAreaState, LandingArea ) -> List (Html Msg)
landingAreaView msg ( landingAreaState, _ ) =
    [ h1 [] [ text "Core Catcher" ] ]
        ++ case landingAreaState of
            Landing ->
                [ text "Connecting..." ]

            LandingConnected ->
                [ Html.form [ onSubmit DoLogin ]
                    [ input
                        [ placeholder "Username"
                        , autofocus True
                        , contenteditable False
                        , onInput PlayerNameChange
                        ]
                        []
                    , button [ type_ "submit" ] [ text "Login" ]
                    ]
                ]

            LoginPending_ _ ->
                [ Html.form [ onSubmit DoLogin ]
                    [ input
                        [ placeholder "Username"
                        , autofocus True
                        , contenteditable False
                        ]
                        []
                    , button [ type_ "submit", disabled True ] [ text "Login..." ]
                    ]
                ]

            LoginFailed ->
                [ text "Login Failed" ]

            LoggedOut ->
                [text "Logged Out"]


loggedInView : ClientModel -> ( LoggedInState, LoggedIn ) -> List (Html Msg)
loggedInView model ( loggedInState, loggedIn ) =
    [ h1 [] [ text "Core Catcher -- Home" ]
    , p [] [ text <| "Logged in as " ++ loggedIn.player.playerName ]
    ]
        ++ case loggedInState of
            InPlayerHome ->
                [ div []
                    -- game lobbies
                    (case loggedIn.playerHomeContent.activeLobbies of
                        Nothing ->
                            []

                        Just [] ->
                            [ text "No active game lobbies" ]

                        Just lobbies ->
                            text "Game Lobbies"
                                :: [ table [] <|
                                        List.map lobbyPreview lobbies
                                   ]
                    )
                ]
                    ++ [ openNewGameButton, br [] [], playerHomeRefreshButton ]

            GameConnectPending ->
                []

            NewGame ->
                [ createGameButton ]

            NewGamePending ->
                []

            JoinGamePending ->
                [ text "joining game..." ]

            GameOver_ gameOver ->
                [ mapView gameOver.networkModel
                , energyOverview gameOver.networkModel
                ]


lobbyPreview : GameLobbyPreview -> Html Msg
lobbyPreview lobby =
    tr [ style [ ( "border", "1px solid" ) ] ]
        [ td [] [ text <| toString lobby.gameLobbyPreviewGameId.gameId ]
        , td [] [ text lobby.gameLobbyPreviewGameName ]
        , td [] [ button [ onClick <| DoJoinGame lobby.gameLobbyPreviewGameId ] [ text "Join" ] ]
        ]


inGameView : ClientModel -> ( InGameState, InGame ) -> List (Html Msg)
inGameView model ( inGameState, inGame ) =
    case inGameState of
        InPreGameLobby_ lobby ->
            [ h3 [] [ text <| "Lobby for " ++ inGame.gameName ]
            , div []
                [ text "players"
                , table [] <| List.map playerInLobby lobby.players
                ]
            , startGameButton
            ]

        GameActive_ _ gameActive ->
            [ mapView gameActive.networkModel
            , energyOverview gameActive.networkModel
            ]

        GameDisconnected ->
            []

        GameServerReconnected ->
            []


playerInLobby : Player -> Html Msg
playerInLobby p =
    tr [] [ td [] [ text p.playerName ] ]
