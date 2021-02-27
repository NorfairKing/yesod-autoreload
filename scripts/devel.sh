#!/usr/bin/env bash

set -e
set -x

stack build \
  --file-watch --watch-all \
  --exec="./scripts/restart.sh $@" \
  --ghc-options="-freverse-errors -DDEVELOPMENT -O0" \
  --fast \
  --pedantic \
  --no-nix-pure
