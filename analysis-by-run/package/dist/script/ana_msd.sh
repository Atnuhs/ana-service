#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly DIR_ROOT="$(cd "$(dirname "${0}")/.."; pwd)"
readonly FILENAME_EXE="msd_ref.out"
readonly FILENAME_RESULT="msd"
readonly FILE_EXE=""${DIR_ROOT}"/bin/${FILENAME_EXE}"
readonly FILE_RESULT="${DIR_ROOT}/${FILENAME_RESULT}"

echo "##### ana => ${FILENAME_EXE}"
echo "##### dir => ${DIR_ROOT}"
mkdir -p "${FILE_RESULT}"
cd "${DIR_ROOT}"
"${FILE_EXE}"
