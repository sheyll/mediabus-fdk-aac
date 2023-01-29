#!/usr/bin/env bash

set -e
nix run .\#prof-mediabus-fdk-aac:exe:mediabus-fdk-aac-example -- $@ +RTS -p -s
