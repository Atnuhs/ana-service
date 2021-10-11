#!/bin/bash

readonly DIR_ROOT="$(cd "$(dirname "${0}")"; pwd)"
readonly DIR_BUILD="${DIR_ROOT:?}/build"
# readonly DIR_FORTRAN_COMPILER='/opt/rh/devtoolset-8/root/usr/bin/gfortran'

mkdir -p "${DIR_BUILD:?}"
cd "${DIR_BUILD:?}"

cmake .. \
# -DCMAKE_Fortran_COMPILER="${DIR_FORTRAN_COMPILER:?}" \
-DCMAKE_BUILD_TYPE=release
# -DCMAKE_Fortran_COMPILER=/opt/intel/compilers_and_libraries_2016.4.258/linux/bin/intel64/ifort \
make
