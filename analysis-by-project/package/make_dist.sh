#!/bin/bash

readonly DIR_ROOT="$(cd "$(dirname "${0}")"; pwd)"
readonly DIR_DIST="${DIR_ROOT:?}/dist"
readonly DIR_SCRIPT="${DIR_ROOT:?}/exec-script"
readonly DIR_SRC="${DIR_ROOT:?}/build/src"


mkdir -p "${DIR_DIST:?}/"{bin,script}
rsync -ah --delete "${DIR_SRC:?}"/*.out "${DIR_DIST:?}/bin"
rsync -ah --delete "${DIR_SCRIPT}/" "${DIR_DIST}/script"
