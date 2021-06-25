#!/bin/bash

echo $0
readonly DIR_ROOT="$(cd "$(dirname "${0}")/.."; pwd)"
readonly FILENAME_EXE="thcd_ref.out"
readonly FILENAME_RESULT="thcd"
readonly FILE_EXE=""${DIR_ROOT:?}"/exe/${FILENAME_EXE:?}"
readonly FILE_RESULT="${DIR_ROOT:?}/${FILENAME_RESULT:?}"
readonly LEFT_CALC_STEP="$1"
readonly RIGHT_CALC_STEP="$2"

echo "##### ana => ${FILENAME_EXE:?}"
echo "##### dir => ${DIR_ROOT:?}"
mkdir -p "${FILENAME_RESULT:?}"
cd "${DIR_ROOT:?}"
# ./exe/"${FILE_EXE##*/}" <<< "${LEFT_CALC_STEP:?} ${RIGHT_CALC_STEP:?}"
"${FILE_EXE:?}" <<< "${LEFT_CALC_STEP:?} ${RIGHT_CALC_STEP:?}"
