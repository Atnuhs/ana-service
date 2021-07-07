#!/bin/bash

readonly DIR_ANALYSIS="$(cd "$(dirname "${0}")/.."; pwd)"
readonly DIR_RATE="$(cd "$(dirname "${0}")/../.."; pwd)"

readonly FILENAME_EXE="msd.out"
readonly DIR_CALCULATION="${DIR_RATE:?}/calculation"

readonly FILE_EXE=""${DIR_ANALYSIS:?}"/bin/${FILENAME_EXE:?}"
readonly DIRNAME_RESULT="msd"
readonly DIR_RESULT="${DIR_ANALYSIS:?}/${DIRNAME_RESULT:?}"
readonly FILE_ANALYSIS_RUN="${DIR_ANALYSIS:?}/analysis_run.txt"


read fst_run lst_run < "${FILE_ANALYSIS_RUN:?}"
echo "##### dir => ${DIR_ANALYSIS:?}"
mkdir -p "${DIR_RESULT:?}"

cd "${DIR_ANALYSIS:?}"
"${FILE_EXE:?}" <<<"${fst_run:?} ${lst_run:?}"