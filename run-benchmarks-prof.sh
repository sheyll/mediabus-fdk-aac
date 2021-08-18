#!/usr/bin/env bash

set -e

HERE=$(realpath $(dirname "$0"))

THERE=$(mktemp --tmpdir -u mediabus-fdk-aac-benchmarks-prof-XXX)

nix build -L -f $HERE/default.nix mediabus-fdk-aac.components.benchmarks.encoder-benchmark -o ${THERE} --arg withProfiling true
${THERE}/bin/encoder-benchmark $@ +RTS -p -s