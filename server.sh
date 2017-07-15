#!/bin/bash

set -e
set -x

host=$1
if [[ -z host ]]; then
  host="localhost"
fi
stack_pid=
elm_pid=
trap 'set -x; kill $stack_pid; kill $elm_pid; echo; exit' INT

stack build
stack test
stack exec elm-bridge
elm-package install --yes
elm-make frontend/Main.elm --output web/elm.js

stack exec core-catcher-exe &
stack_pid=$!

elm-live frontend/Main.elm --output web/elm.js --host=$host --dir=web --yes &
elm_pid=$!

set +x

echo "stack: $stack_pid"
echo "elm: $elm_pid"

while read p ; do
  true
done

kill $stack_pid
kill $elm_pid


