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
import Experimental.DebugView exposing (debugView)

view : ClientModel -> Html Msg
view state =
    div []
        ([]
            ++ [ hr [] [], debugView state]
        )
