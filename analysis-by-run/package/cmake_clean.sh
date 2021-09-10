#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly DIR_ROOT="$(cd "$(dirname "${0}")"; pwd)"

rm -rf "${DIR_ROOT}/build"/* 2> /dev/null
rm -f "${DIR_ROOT}/src"/*.mod 2> /dev/null
rm -f "${DIR_ROOT}/lib"/*.mod 2> /dev/null
