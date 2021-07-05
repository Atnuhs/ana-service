#!/bin/bash

readonly DIR_ROOT="$(cd "$(dirname "${0}")/.."; pwd)"
readonly FILENAME_EXE="integ_acf_ej.out"
readonly FILENAME_RESULT="thcd"
readonly FILE_EXE=""${DIR_ROOT:?}"/bin/${FILENAME_EXE:?}"
readonly FILE_RESULT="${DIR_ROOT:?}/${FILENAME_RESULT:?}"

echo "##### ana => ${FILENAME_EXE:?}"
echo "##### dir => ${DIR_ROOT:?}"
mkdir -p "${FILE_RESULT:?}"
cd "${DIR_ROOT:?}"
"${FILE_EXE:?}"
