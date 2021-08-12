#!/usr/bin/env bash

set -e

HERE=$(realpath $(dirname "$0"))

THERE=$(mktemp --tmpdir -u mediabus-fdk-aac-examples-prof-XXX)

nix build -L -f $HERE/default.nix mediabus-fdk-aac.components.tests.examples -o $THERE  --arg withProfiling true
${THERE}/bin/examples $@ +RTS -p -s
