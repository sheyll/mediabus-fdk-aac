#!/usr/bin/env bash

set -e

nix run .\#prof-mediabus-fdk-aac.components.tests.tests -- $@ +RTS -p -s
