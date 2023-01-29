#!/usr/bin/env bash

set -e

nix run .\#prof-mediabus-fdk-aac:bench:encoder-benchmark -- $@ +RTS -p -s
