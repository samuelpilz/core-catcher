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


view : ClientModel -> Html Msg
view state =
    div []
        (stateView state
            ++ [ hr [] [], div [] [ text <| toString state ], allMsgView ]
        )


stateView : ClientModel -> List (Html Msg)
stateView state =
    case state.state of
        LandingArea_ landingAreaState landingArea ->
            viewLandingArea state ( landingAreaState, landingArea )

        LoggedIn_ loggedInState loggedIn ->
            viewLoggedIn state ( loggedInState, loggedIn )

        InGame_ inGameState inGame ->
            viewInGame state ( inGameState, inGame )

        Disconnected disconnected ->
            [ serverHelloButton, toLandingPageButton ]

        Reconnected reconnected ->
            [ connectionLostButton, serverHelloButton, loginSuccessButton, loginFailButton ]


viewLandingArea : ClientModel -> ( LandingAreaState, LandingArea ) -> List (Html Msg)
viewLandingArea msg ( landingAreaState, _ ) =
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
                , loginSuccessButton
                , loginFailButton
                ]

            LoginFailed ->
                [ connectionLostButton
                , toLandingPageButton
                ]


viewLoggedIn : ClientModel -> ( LoggedInState, LoggedIn ) -> List (Html Msg)
viewLoggedIn model ( loggedInState, _ ) =
    [ serverHelloButton, connectionLostButton, toHomeButton ]
        ++ case loggedInState of
            PlayerHome ->
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

            PreGameLobby ->
                [ initialInfoForGameButton
                ]

            GameOver ->
                [ ]


viewInGame : ClientModel -> ( InGameState, InGame ) -> List (Html Msg)
viewInGame model ( inGameState, _ ) =
    [ serverHelloButton, connectionLostButton ]
        ++ case inGameState of
            GameActive_ YourTurn ->
                [ actionButton
                , gameViewButton
                , gameOverButton
                ]

            GameActive_ ActionPending ->
                [ gameViewButton
                , gameErrorButton
                , gameOverButton
                ]

            GameActive_ OthersTurn ->
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
            , loginSuccessButton
            , loginFailButton
            ]
        , p []
            [ toHomeButton
            , gameConnectButton
            , joinGameButton
            , openNewGameButton
            , preGameLobbyButton
            ]
        , p []
            [ initialInfoForGameButton
            , actionButton
            , gameViewButton
            , gameOverButton
            , gameErrorButton
            ]
        ]
