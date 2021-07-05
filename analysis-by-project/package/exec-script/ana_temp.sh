#!/bin/bash

readonly FST_RUN="${1:?}"
readonly LST_RUN="${2:?}"

readonly DIR_ANALYSIS="$(cd "$(dirname "${0}")/.."; pwd)"
readonly DIR_RATE="$(cd "$(dirname "${0}")/../.."; pwd)"

readonly DIR_CALCULATION="${DIR_RATE:?}/calculation"

readonly FILENAME_RESULT="temp"
readonly FILE_RESULT="${DIR_ANALYSIS:?}/${FILENAME_RESULT:?}"

echo "##### ana => ${FILENAME_EXE:?}"
echo "##### dir => ${DIR_ANALYSIS:?}"
mkdir -p "${FILE_RESULT:?}"

for run in "$(seq "${FST_RUN:?}" "${LST_RUN:?}")"
do
    readonly DIR_RUN_ANA="${DIR_CALCULATION:?}/${run:?}/Analysis"
    readonly FILE_RUN_TEMP_MEAN="${DIR_RUN_ANA}/temp/temp_mean.dat"
    read temp temp_sd < "${FILE_RUN_TEMP_MEAN}"
    echo "run${run:?}: temp=${temp:?}: temp_sd=${temp_sd:?}"
done
