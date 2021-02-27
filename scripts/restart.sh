#!/usr/bin/env bash

set -x
set -e

killall yesod-autoreload-example || true
yesod-autoreload-example serve $@ &
