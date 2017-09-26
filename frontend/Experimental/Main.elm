module Experimental.Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import View.MapView exposing (mapView)
import Debug exposing (log)
import Example.ExampleGameViewDisplay as Example
import Protocol exposing (..)
import ProtocolUtils exposing (..)
import Experimental.ClientState exposing (..)
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)
import AllDict as AllDict
import EveryDict
import AllDict exposing (AllDict)
import Navigation exposing (..)
import AnimationFrame exposing (diffs)
import ProtocolEmpty exposing (..)
import Navigation exposing (program)
import Experimental.Update exposing (..)
import Experimental.View exposing (..)

init : Location -> ( ClientModel, Cmd Msg )
init location =
    { state = LandingArea_ Landing { playerNameField = "" }, server = location.hostname } ! []


-- UPDATE FUNCTIONS

const : a -> b -> a
const a b =
    a


log2 : String -> a -> b -> b
log2 s a b =
    const b (log s a)
