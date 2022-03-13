#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

DIR_FORTRAN_COMPILER="$(which gfortran)"

mkdir -p "${DIR_BUILD}"
cd "${DIR_BUILD}"

cmake .. \
-DCMAKE_Fortran_COMPILER="${DIR_FORTRAN_COMPILER}" \
-DCMAKE_BUILD_TYPE=release
make
