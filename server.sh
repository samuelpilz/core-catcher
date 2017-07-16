#!/bin/bash

set -e

host=$1
if [[ -z host ]]; then
  host="localhost"
fi
# TODO: trap
# trap 'echo "Be patient"' INT

stack test
elm-package install --yes

stack exec core-catcher-exe &
stack_pid=$!

elm-live frontend/Main.elm --output web/elm.js --host=$host --dir=web --yes &
elm_pid=$!

while read p ; do
  true
done

echo "stack: $stack_pid"
echo "elm: $elm_pid"

kill $stack_pid
kill $elm_pid

cd ..

