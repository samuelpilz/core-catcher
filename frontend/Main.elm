module Main exposing (..)

import Html as Html exposing (Html, program, div, button, text)
import Html.Events as Events exposing (onClick)


type Msg
    = Inc
    | Dec


type alias Model =
    Maybe Int


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd msg )
init =
    ( Just 0, Cmd.none )


view : Model -> Html Msg
view num =
    let
        n =
            num
                |> Maybe.andThen (\m -> Just <| m * m)

        res =
            Maybe.withDefault 0 n
    in
        div
            []
            [ button [ onClick Inc ] [ text "Click me" ]
            , div [] [ text (toString <| res) ]
            , button [ onClick Dec ] [ text "-" ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Inc ->
            let
                n =
                    Maybe.withDefault 0 model
            in
                ( Just <| n + 1, Cmd.none )

        Dec ->
            ( Nothing, Cmd.none )
