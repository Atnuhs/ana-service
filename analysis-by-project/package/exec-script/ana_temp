#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly FILENAME_EXE=""
readonly DIRNAME_RESULT="temp"

readonly DIR_SCRIPT="$(cd "$(dirname "${0}")"; pwd)"
readonly DIR_LIB="${DIR_SCRIPT}/lib"

. "${DIR_LIB}/common.sh"

readonly FILE_TEMP_MEAN="${DIR_RESULT:?}/temp_mean.txt"
readonly FILE_TEMP_ALL="${DIR_RESULT:?}/temp_all.txt"

read fst_run lst_run < "${FILE_ANALYSIS_RUN:?}"
echo "##### dir => ${DIR_ANALYSIS:?}"
mkdir -p "${DIR_RESULT:?}"

echo -e 'run_number\ttemperature\ttemperature_sd' | tee "${FILE_TEMP_ALL:?}"

for run in $(seq -f '%02g' "${fst_run:?}" "${lst_run:?}")
do
    dir_run_ana="${DIR_CALCULATION:?}/run${run:?}/Analysis"
    file_run_temp_mean="${dir_run_ana:?}/temp/temp_mean.dat"
    read temp temp_sd < "${file_run_temp_mean}"
    echo -e "run${run:?}\t${temp:?}\t${temp_sd:?}"
done | tee -a "${FILE_TEMP_ALL:?}"

# 平均の計算

tail -n +2 "${FILE_TEMP_ALL:?}" | \
awk '{print $2}' | \
awk -f "${FILE_STDDEV:?}" > "${FILE_TEMP_MEAN:?}"
