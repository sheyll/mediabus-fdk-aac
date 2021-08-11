#!/usr/bin/env bash

set -e

nix-build -A mediabus-fdk-aac.components.tests.examples $@
