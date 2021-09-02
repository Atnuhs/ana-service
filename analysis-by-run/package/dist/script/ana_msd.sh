#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly DIR_ROOT="$(cd "$(dirname "${0}")/.."; pwd)"
readonly FILENAME_EXE="msd.out"
readonly FILENAME_RESULT="msd"
readonly FILE_EXE=""${DIR_ROOT}"/bin/${FILENAME_EXE}"
readonly FILE_RESULT="${DIR_ROOT}/${FILENAME_RESULT}"

mkdir -p "${FILE_RESULT}"
cd "${DIR_ROOT}"
"${FILE_EXE}"
