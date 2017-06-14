module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Debug exposing (log)
import Regex exposing (..)


type alias Model = 
    { pokeCount : Int
    , fieldText : String
    , fieldValue : Maybe Int
    }

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
    ( {pokeCount = 0, fieldText = "1", fieldValue = Just 1}, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text <| "Pokes: " ++ toString model.pokeCount ]
        , div []
            [ input [ value model.fieldText, onInput UpdateNum ] []
            , button [ onClick Send ] [ text "Poke others" ]
            ]
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
                {model | pokeCount = model.pokeCount + getNumberFromPokeString m } ! []
            else
                model ! []

        Send ->
            case model.fieldValue of
                Just p ->
                    model ! [ WebSocket.send wsUrl ("poke" ++ toString p ) ]

                Nothing ->
                    model ! []
            
        UpdateNum s ->
            {model | fieldText = s , fieldValue = Result.toMaybe <| String.toInt <| s} ! []

        Clicked n ->
            model ! []


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
