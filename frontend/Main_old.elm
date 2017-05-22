module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import MapView exposing (mapView)
import Debug exposing (log)
import Regex exposing (..)


type alias Model =
    ( Int, Result String Int)


type Msg
    = Receive String
    | Send
    | UpdateNum String
    | Clicked Int

main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( ( 0, Ok 1 ), Cmd.none )


view : Model -> Html Msg
view ( poked, pokingState ) =
    div []
        [ p [] [ text <| "Pokes: " ++ toString poked ]
        , div []
            [ input [ value (printPokingState pokingState), onInput UpdateNum ] []
            , button [ onClick Send ] [ text "Poke others" ]
            ]
        , MapView.mapView
        ]


wsUrl : String
wsUrl =
    "ws://localhost:3000"


subscriptions : Model -> Sub Msg
subscriptions _ =
    WebSocket.listen wsUrl Receive


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case log "msg" msg of
        Receive m ->
            if contains pokeRegex m then
                ( Tuple.first model + getNumberFromPokeString m, Tuple.second model ) ! []
            else
                model ! []

        Send ->
            model ! [ WebSocket.send wsUrl ("poke" ++ printPokingState (Tuple.second model)) ]

        UpdateNum s ->
            ( Tuple.first model, handleInputString s ) ! []

        Clicked n ->
            model ! []


handleInputString : String -> Result String Int
handleInputString s =
    case String.toInt s of
        Ok i ->
            Ok i

        Err _ ->
            Err s


printPokingState : Result String Int -> String
printPokingState x =
    case x of
        Ok i ->
            toString i

        Err s ->
            s


pokeRegex : Regex
pokeRegex =
    regex "poke(-?\\d+)"


getNumberFromPokeString : String -> Int
getNumberFromPokeString s =
    let
        match : Maybe Regex.Match
        match =
            List.head <| find (AtMost 1) pokeRegex s

        extractedString : Maybe String
        extractedString =
            Maybe.andThen identity
                << Maybe.andThen (\m -> List.head m.submatches)
            <|
                match

        extractedNumber : Maybe Int
        extractedNumber =
            Maybe.andThen (\x -> Result.toMaybe << String.toInt <| x) extractedString

        number : Int
        number =
            Maybe.withDefault -1 extractedNumber
    in
        number
