#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly DIR_ROOT="$(cd "$(dirname "${0}")"; pwd)"
readonly DIR_DIST="${DIR_ROOT}/dist"
readonly DIR_SCRIPT="${DIR_ROOT}/exec-script"
readonly DIR_SRC="${DIR_ROOT}/build/src"


mkdir -p "${DIR_DIST}"
rsync -ah --delete "${DIR_SRC}"/*.out "${DIR_DIST}/bin"
rsync -ah --delete "${DIR_SCRIPT}/" "${DIR_DIST}/script"
