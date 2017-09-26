#!/bin/bash

set -e
set -x

stack build
stack test

stack exec elm-bridge
elm-make frontend/Main.elm --output web/elm.js --yes

stack exec core-catcher-exe

