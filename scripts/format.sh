#!/usr/bin/env bash

# This script formats Haskell packages in the "./packages/" directory with using
# the stylish-haskell formatter.

set -euo pipefail
IFS=$'\n\t'

################################################################################

# Path to the stylish-haskell configuration file.
CONFIG=".stylish-haskell.yaml"

# Path to the directory containing Haskell packages to be formatted.
PACKAGES="./."

function FORMAT() {
  stylish-haskell \
    --config "${CONFIG}" \
    --recursive \
    --inplace \
    "${PACKAGES}"
}

FORMAT