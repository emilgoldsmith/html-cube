#!/bin/bash

set -euo pipefail

ROOT_DIRECTORY=$(dirname "${BASH_SOURCE[0]}")/../..

cd $ROOT_DIRECTORY

elm make src/Algorithm src/Cube src/PLL src/Cube/Advanced src/Cube/Advanced/Types --output=/dev/null
