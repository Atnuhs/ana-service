#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

. "$(dirname $0)/lib/common.sh"

rm -rf "${DIR_BUILD}"
rm -rf "${DIR_DIST}"
rm -f "${DIR_SRC}"/*.mod
rm -f "${DIR_LIB}"/*.mod
