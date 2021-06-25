#!/bin/bash

readonly DIR_ROOT="$(cd "$(dirname "${0}")"; pwd)"
readonly DIR_SRC="${DIR_ROOT:?}/build/src"
readonly DIR_DIST="${DIR_ROOT:?}/exe"


mkdir -p "${DIR_DIST:?}"
cp "${DIR_SRC:?}"/*.out "${DIR_DIST:?}"
