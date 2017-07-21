#!/bin/bash

set -e
set -x

stack build
stack test

stack exec elm-bridge
elm-package install --yes
elm-make frontend/Main.elm --output web/elm.js

stack exec core-catcher-exe

