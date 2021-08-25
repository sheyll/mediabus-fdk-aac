#!/usr/bin/env bash

set -e

HERE=$(realpath $(dirname "$0"))

THERE=$(mktemp --tmpdir -u mediabus-fdk-aac-tests-XXX)

nix build -L -f $HERE/default.nix mediabus-fdk-aac.components.tests.tests -o $THERE
${THERE}/bin/tests $@
