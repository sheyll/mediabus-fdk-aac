#!/usr/bin/env bash

set -e

nix run .\#mediabus-fdk-aac:bench:encoder-benchmark -- $@