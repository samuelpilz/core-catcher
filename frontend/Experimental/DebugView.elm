module Experimental.DebugView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Protocol exposing (..)
import ProtocolEmpty exposing (..)
import Experimental.ClientState exposing (..)
import AllDict exposing (..)
import EveryDict
import Experimental.MsgButtons exposing (..)


debugView : ClientModel -> Html Msg
debugView state =
    div []
        (manualMsgView state
            ++ [ hr [] [] ]
            ++ possibleMsgView state
            ++ [ hr [] [], div [] [ text <| toString state ], allMsgView ]
        )


manualMsgView : ClientModel -> List (Html Msg)
manualMsgView state =
    case state.state of
        LandingArea_ landingAreaState landingArea ->
            manualMsgLandingArea state ( landingAreaState, landingArea )

        LoggedIn_ loggedInState loggedIn ->
            manualMsgLoggedIn state ( loggedInState, loggedIn )

        InGame_ inGameState inGame ->
            manualMsgInGame state ( inGameState, inGame )

        Disconnected disconnected ->
            [ serverHelloButton, toLandingPageButton ]

        Reconnected reconnected ->
            [ connectionLostButton, serverHelloButton, playerHomeButton, loginFailButton ]


manualMsgLandingArea : ClientModel -> ( LandingAreaState, LandingArea ) -> List (Html Msg)
manualMsgLandingArea msg ( landingAreaState, _ ) =
    case landingAreaState of
        Landing ->
            [ text "Waiting for connection" ]

        LandingConnected ->
            [ loginButton
            ]

        LoginPending_ _ ->
            [ text "waiting for login" ]

        LoginFailed ->
            [ toLandingPageButton
            ]

        LoggedOut ->
                []


manualMsgLoggedIn : ClientModel -> ( LoggedInState, LoggedIn ) -> List (Html Msg)
manualMsgLoggedIn model ( loggedInState, _ ) =
    [ toHomeButton, logoutButton ]
        ++ case loggedInState of
            InPlayerHome ->
                [ gameConnectButton
                , joinGameButton
                , openNewGameButton
                ]

            GameConnectPending ->
                []

            NewGame ->
                [ createGameButton
                ]

            NewGamePending ->
                []

            JoinGamePending ->
                []

            GameOver_ _ ->
                []


manualMsgInGame : ClientModel -> ( InGameState, InGame ) -> List (Html Msg)
manualMsgInGame model ( inGameState, _ ) =
    case inGameState of
        InPreGameLobby_ _ ->
            [ startGameButton ]

        GameActive_ YourTurn _ ->
            [ actionButton ]

        GameActive_ ActionPending _ ->
            [ text "waiting for action-response" ]

        GameActive_ OthersTurn _ ->
            [ text "waiting for others" ]

        GameDisconnected ->
            [ text "disconnected" ]

        GameServerReconnected ->
            [ text "reconnected" ]


possibleMsgView : ClientModel -> List (Html Msg)
possibleMsgView state =
    case state.state of
        LandingArea_ landingAreaState landingArea ->
            possibleMsgLandingArea state ( landingAreaState, landingArea )

        LoggedIn_ loggedInState loggedIn ->
            possibleMsgLoggedIn state ( loggedInState, loggedIn )

        InGame_ inGameState inGame ->
            possibleMsgInGame state ( inGameState, inGame )

        Disconnected disconnected ->
            [ serverHelloButton, toLandingPageButton ]

        Reconnected reconnected ->
            [ connectionLostButton, serverHelloButton, playerHomeButton, loginFailButton ]


possibleMsgLandingArea : ClientModel -> ( LandingAreaState, LandingArea ) -> List (Html Msg)
possibleMsgLandingArea msg ( landingAreaState, _ ) =
    [ serverHelloButton ]
        ++ case landingAreaState of
            Landing ->
                []

            LandingConnected ->
                [ connectionLostButton
                , loginButton
                ]

            LoginPending_ _ ->
                [ connectionLostButton
                , playerHomeButton
                , loginFailButton
                ]

            LoginFailed ->
                [ connectionLostButton
                , toLandingPageButton
                ]

            LoggedOut ->
                []


possibleMsgLoggedIn : ClientModel -> ( LoggedInState, LoggedIn ) -> List (Html Msg)
possibleMsgLoggedIn model ( loggedInState, _ ) =
    [ serverHelloButton, connectionLostButton, toHomeButton, logoutButton ]
        ++ case loggedInState of
            InPlayerHome ->
                [ gameConnectButton
                , joinGameButton
                , openNewGameButton
                ]

            GameConnectPending ->
                [ initialInfoForGameButton
                , gameOverButton
                ]

            NewGame ->
                [ createGameButton
                ]

            NewGamePending ->
                [ preGameLobbyButton
                ]

            JoinGamePending ->
                [ preGameLobbyButton
                ]

            GameOver_ _ ->
                []


possibleMsgInGame : ClientModel -> ( InGameState, InGame ) -> List (Html Msg)
possibleMsgInGame model ( inGameState, _ ) =
    [ serverHelloButton, connectionLostButton ]
        ++ case inGameState of
            InPreGameLobby_ _ ->
                [ startGameButton
                , initialInfoForGameButton
                ]

            GameActive_ YourTurn _ ->
                [ actionButton
                , gameViewButton
                , gameOverButton
                ]

            GameActive_ ActionPending _ ->
                [ gameViewButton
                , gameErrorButton
                , gameOverButton
                ]

            GameActive_ OthersTurn _ ->
                [ gameViewButton
                ]

            GameDisconnected ->
                []

            GameServerReconnected ->
                [ initialInfoForGameButton
                , gameOverButton
                , loginFailButton
                ]


allMsgView : Html Msg
allMsgView =
    div []
        [ p []
            [ connectionLostButton
            , serverHelloButton
            , toLandingPageButton
            ]
        , p []
            [ loginButton
            , playerHomeButton
            , loginFailButton
            ]
        , p []
            [ toHomeButton
            , gameConnectButton
            , joinGameButton
            , openNewGameButton
            , preGameLobbyButton
            , startGameButton
            ]
        , p []
            [ initialInfoForGameButton
            , actionButton
            , gameViewButton
            , gameOverButton
            , gameErrorButton
            ]
        ]
