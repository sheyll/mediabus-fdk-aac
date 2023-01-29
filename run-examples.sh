#!/usr/bin/env bash

set -e

nix run .\#mediabus-fdk-aac:exe:mediabus-fdk-aac-example -- $@
