module View.EnergyView exposing (energyView)

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
import View.GameViewDisplay exposing (..)
import Maybe exposing (..)
import Tuple as Tuple


energyView : Network -> GameViewDisplayInfo -> GameState -> Html.Html Msg
energyView _ displayInfo clientState =
    svg
        [ height (toString displayInfo.mapHeight)
        , width "400"
        , Html.style [ ( "border-size", "1" ) ]
        ]
    <|
        availableEnergyView
            clientState.playerEnergies
            displayInfo
            clientState.player
            clientState.selectedEnergy
            ++ historyView clientState.rogueHistory displayInfo


availableEnergyView : PlayerEnergies -> GameViewDisplayInfo -> Player -> Energy -> List (Svg.Svg Msg)
availableEnergyView playerEnergies displayInfo player selectedEnergy =
    List.map (energyRecord selectedEnergy)
        << List.sortBy (\( priority, _, _, _ ) -> priority)
        << List.map
            (\( energy, color ) ->
                ( getPriority displayInfo energy
                , energy
                , color
                , getEnergyForEnergyAndPlayer player energy playerEnergies
                )
            )
        << AllDict.toList
    <|
        displayInfo.colorMap


historyView : RogueHistory -> GameViewDisplayInfo -> List (Svg.Svg Msg)
historyView rogueHistory displayInfo =
    List.map2 historyRecord (range 0 100)
        << List.map
            (\( t, n ) ->
                ( Maybe.withDefault "black" << AllDict.get t <| displayInfo.colorMap
                , n
                )
            )
        << List.reverse
    <|
        rogueHistory.rogueHistory


energyRecord : Energy -> ( Int, Energy, Color, Int ) -> Svg.Svg Msg
energyRecord selectedEnergy ( pos, energy, color, count ) =
    g []
        [ rect
            [ x << toString <| 50 * pos + 50
            , y << toString <| 3
            , width "49"
            , height "49"
            , Svg.Attributes.cursor "pointer"
            , fill color
            , stroke "#000000"
            , strokeWidth
                << toString
              <|
                if selectedEnergy == energy then
                    3
                else
                    1
            , onClick (SelectEnergy energy)
            ]
            []
        , text_
            [ x << toString <| 50 * pos + 75
            , y << toString <| 30
            , fill "#ffffff"
            , Svg.Attributes.cursor "pointer"
            , onClick (SelectEnergy energy)
            , textAnchor "middle"
            ]
            [ text << toString <| count ]
        ]


historyRecord : Int -> ( Color, Maybe Node ) -> Svg.Svg Msg
historyRecord pos ( color, nodeMay ) =
    g []
        [ rect
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
        , text_
            [ x << toString <| 50 + 50 * (pos % 5) + 25
            , y << toString <| 100 + 50 * (pos // 5) + 27
            , fill "#ffffff"
            , Svg.Attributes.cursor "pointer"
            , textAnchor "middle"
            ]
            [ text
                << Maybe.withDefault ""
                << Maybe.map toString
                << Maybe.map .nodeId
              <|
                nodeMay
            ]
        ]
