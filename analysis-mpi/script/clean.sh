#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

. "$(dirname "$0")/lib/common.sh"


rm -rf "${DIR_ROOT}/build"
rm -f "${DIR_ROOT}/src"/*.mod
rm -f "${DIR_ROOT}/lib"/*.mod
