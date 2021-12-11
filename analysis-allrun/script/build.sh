#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

. "$(dirname $0)/lib/common.sh"

readonly DIR_FORTRAN_COMPILER='/opt/rh/devtoolset-8/root/usr/bin/gfortran' # mdz11
# readonly DIR_FORTRAN_COMPILER='/opt/intel/compilers_and_libraries_2016.4.258/linux/bin/intel64/ifort' # wsk2

mkdir -p "${DIR_BUILD}"
cd "${DIR_BUILD}"

cmake .. \
-DCMAKE_Fortran_COMPILER="${DIR_FORTRAN_COMPILER}" \
-DCMAKE_BUILD_TYPE=release
make
