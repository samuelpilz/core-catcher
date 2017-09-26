module Experimental.MsgButtons exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Protocol exposing (..)
import ProtocolEmpty exposing (..)
import Experimental.ClientState exposing (..)


serverHelloButton : Html Msg
serverHelloButton =
    button [ onClick <| MsgFromServer <| ServerHello ] [ text "Server Hello" ]


connectionLostButton : Html Msg
connectionLostButton =
    button [ onClick <| ConnectionLost ] [ text "Connection Lost" ]


toLandingPageButton : Html Msg
toLandingPageButton =
    button [ onClick <| ToLandingPage ] [ text "To Landing page" ]


loginButton : Html Msg
loginButton =
    button [ onClick <| DoLogin ] [ text "Login" ]


loginSuccessButton : Html Msg
loginSuccessButton =
    button [ onClick <| MsgFromServer <| LoginSuccess_ emptyLoginSuccess ] [ text "Login Sucess" ]


loginFailButton : Html Msg
loginFailButton =
    button [ onClick <| MsgFromServer <| LoginFail_ emptyLoginFail ] [ text "Login Fail" ]


toHomeButton : Html Msg
toHomeButton =
    button [ onClick <| ToHome ] [ text "To Home" ]


gameConnectButton : Html Msg
gameConnectButton =
    button [ onClick <| DoGameConnect ] [ text "Game Connect" ]


joinGameButton : Html Msg
joinGameButton =
    button [ onClick <| DoJoinGame ] [ text "Join Game " ]


openNewGameButton : Html Msg
openNewGameButton =
    button [ onClick <| DoOpenNewGame ] [ text "Open New Game" ]


createGameButton : Html Msg
createGameButton =
    button [ onClick <| DoCreateGame ] [ text "Create Game" ]


preGameLobbyButton : Html Msg
preGameLobbyButton =
    button [ onClick <| MsgFromServer <| PreGameLobby_ ] [ text "PreGame Lobby" ]


initialInfoForGameButton : Html Msg
initialInfoForGameButton =
    button [ onClick <| MsgFromServer <| InitialInfoForGame_ emptyInitialInfo ] [ text "Initial Game Info" ]


actionButton : Html Msg
actionButton =
    button [ onClick <| GameAction_ <| Movement emptyNode ] [ text "Action" ]


gameErrorButton : Html Msg
gameErrorButton =
    button [ onClick <| MsgFromServer <| GameError_ emptyError ] [ text "Game Error" ]


gameViewButton : Html Msg
gameViewButton =
    button [ onClick <| MsgFromServer <| GameView_ emptyGameView ] [ text "Game View" ]


gameOverButton : Html Msg
gameOverButton =
    button [ onClick <| MsgFromServer <| GameOverView_ emptyGameOverView ] [ text "Game Over" ]
