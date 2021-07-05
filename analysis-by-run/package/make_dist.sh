#!/bin/bash

readonly DIR_ROOT="$(cd "$(dirname "${0}")"; pwd)"
readonly DIR_DIST="${DIR_ROOT:?}/dist"
readonly DIR_SCRIPT="${DIR_ROOT:?}/exec-script"
readonly DIR_SRC="${DIR_ROOT:?}/build/src"


mkdir -p "${DIR_DIST:?}/"{bin,script}
cp "${DIR_SRC:?}"/*.out "${DIR_DIST:?}/bin"
cp -RT "${DIR_SCRIPT}" "${DIR_DIST}/script"
