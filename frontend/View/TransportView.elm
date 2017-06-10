module View.TransportView exposing (transportView)

import Html as Html
import Html.Attributes as Html
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (..)
import AllDict exposing (..)
import Data exposing (..)
import Protocol exposing (..)
import ProtocolUtils exposing (..)
import GameViewDisplay exposing (..)
import Maybe exposing (..)
import Tuple as Tuple


transportView : Network -> GameViewDisplayInfo -> GameView -> Html.Html Msg
transportView _ displayInfo gameView =
    svg
        [ height (toString displayInfo.mapHeight)
        , width "400"
        , Html.style [ ( "border-size", "1" ) ]
        ]
    -- elements of svg now
    <|
        energyView gameView displayInfo {playerId = 1}
            ++ historyView gameView displayInfo


energyView : GameView -> GameViewDisplayInfo -> Player -> List (Svg.Svg Msg)
energyView gameView displayInfo player =
    List.map energyRecord
        << List.sortBy (\( priority, _, _ ) -> priority)
        << List.map
            (\( transport, color ) ->
                ( getPriority displayInfo transport
                , color
                , getEnergyForTransportAndPlayer player transport gameView
                )
            )
        << AllDict.toList
    <|
        displayInfo.colorMap


historyView : GameView -> GameViewDisplayInfo -> List (Svg.Svg Msg)
historyView gameView displayInfo  =
    List.map2 historyRecord (range 0 100)
        << List.map (Maybe.withDefault "black")
        << List.map (\t -> AllDict.get t displayInfo.colorMap)
    <|
        (rogueHistory gameView).rogueTransportHistory


energyRecord : ( Int, Color, Int ) -> Svg.Svg Msg
energyRecord ( pos, color, count ) =
    g []
        [ rectForPosAndColor ( 50 * pos + 50, 1 ) color
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
    rectForPosAndColor ( 50 + 50 * (pos % 5), 100 + 50 * (pos // 5) ) color


rectForPosAndColor : ( Int, Int ) -> Color -> Svg.Svg Msg
rectForPosAndColor ( xPos, yPos ) color =
    rect
        [ x << toString <| xPos
        , y << toString <| yPos
        , width "49"
        , height "49"
        , Svg.Attributes.cursor "pointer"
        , fill color
        , stroke "#000000"
        , strokeWidth "1"
        ]
        []
