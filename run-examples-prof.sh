#!/usr/bin/env bash

set -e

HERE=$(realpath $(dirname "$0"))

THERE=$(mktemp --tmpdir -u mediabus-fdk-aac-examples-prof-XXX)

nix build -L -f $HERE/default.nix mediabus-fdk-aac.components.exes.mediabus-fdk-aac-example -o $THERE  --arg withProfiling true
${THERE}/bin/mediabus-fdk-aac-example $@ +RTS -p -s
