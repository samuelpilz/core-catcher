module View.TransportView exposing (transportView)

import Html as Html
import Html.Attributes as Html
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)
import AllDict exposing (..)
import ClientState exposing (..)
import Protocol exposing (..)
import ProtocolUtils exposing (..)
import GameViewDisplay exposing (..)
import Maybe exposing (..)
import Tuple as Tuple


transportView : Network -> GameViewDisplayInfo -> ClientState -> Html.Html Msg
transportView _ displayInfo clientState =
    svg
        [ height (toString displayInfo.mapHeight)
        , width "400"
        , Html.style [ ( "border-size", "1" ) ]
        ]
    <|
        energyView clientState.gameView displayInfo { playerId = 0 } clientState.selectedEnergy
            ++ historyView clientState.gameView displayInfo


energyView : GameView -> GameViewDisplayInfo -> Player -> Transport -> List (Svg.Svg Msg)
energyView gameView displayInfo player selectedEnergy =
    List.map (energyRecord selectedEnergy)
        << List.sortBy (\( priority, _, _, _ ) -> priority)
        << List.map
            (\( transport, color ) ->
                ( getPriority displayInfo transport
                , transport
                , color
                , getEnergyForTransportAndPlayer player transport gameView
                )
            )
        << AllDict.toList
    <|
        displayInfo.colorMap


historyView : GameView -> GameViewDisplayInfo -> List (Svg.Svg Msg)
historyView gameView displayInfo =
    List.map2 historyRecord (range 0 100)
        << List.map (Maybe.withDefault "black")
        << List.map (\t -> AllDict.get t displayInfo.colorMap)
    <|
        (rogueHistory gameView).rogueTransportHistory


energyRecord : Transport -> ( Int, Transport, Color, Int ) -> Svg.Svg Msg
energyRecord selectedEnergy ( pos, transport, color, count ) =
    g []
        [ rect
            [ x << toString <| 50 * pos + 50
            , y << toString <| 3
            , width "49"
            , height "49"
            , Svg.Attributes.cursor "pointer"
            , fill color
            , stroke "#000000"
            , strokeWidth << toString <| if selectedEnergy == transport then 3 else 1
            , onClick (SelectEnergy transport)
            ]
            []
        , text_
            [ x << toString <| 50 * pos + 75
            , y << toString <| 30
            , fill "#ffffff"
            , Svg.Attributes.cursor "pointer"

            --, onClick
            , textAnchor "middle"
            ]
            [ text << toString <| count ]
        ]


historyRecord : Int -> Color -> Svg.Svg Msg
historyRecord pos color =
    rect
        [ x << toString <| 50 + 50 * (pos % 5)
        , y << toString <| 100 + 50 * (pos // 5)
        , width "49"
        , height "49"
        , Svg.Attributes.cursor "pointer"
        , fill color
        , stroke "#000000"
        , strokeWidth "1"
        ]
        []
