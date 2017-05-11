module Main exposing (main)

import Html        exposing (..)
import Html.Events exposing (..)
import WebSocket

type alias Model
  = Int

type Msg
  = Receive String
  | Send

main : Program Never Model Msg
main =
  program
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

init : (Model, Cmd Msg)
init =
  (0, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ p [] [ text <| "Pokes: " ++ toString model ]
    , button [ onClick Send ] [ text "Poke others" ]
    ]

wsUrl : String
wsUrl = "ws://localhost:3000"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Receive "poke" ->
      (model + 1) ! []
    Receive _ ->
      model ! []
    Send ->
      model ! [ WebSocket.send wsUrl "poke" ]

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsUrl Receive
