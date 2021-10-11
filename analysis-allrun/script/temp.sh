#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

. "$(dirname $0)/lib/common.sh"

temp () {
    local -r fst_run=$1 lst_run=$2
    local -r file_temp_mean="temp/temp_mean.txt"
    local -r file_temp_all="temp/temp_all.txt"
    local -r header='run_number\ttemperature\ttemperature_sd'

    echo -e ${header} | tee "${file_temp_all}"

    for run in $(seq -f '%02g' "${fst_run}" "${lst_run}")
    do
        dir_run_ana="${DIR_CALCULATION}/run${run}/Analysis"
        file_run_temp_mean="${dir_run_ana}/temp/temp_mean.dat"
        read temp temp_sd < "${file_run_temp_mean}"
        echo -e "run${run}\t${temp}\t${temp_sd}"
    done | tee -a "${file_temp_all}"

    tail -n +2 "${file_temp_all}" |
        awk '{print $2}' |
        awk -f "${FILE_STDDEV}" > "${file_temp_mean}"
}

read fst_run lst_run < "${FILE_ANALYSIS_RUN}"
readonly DIR_OUTPUT=$(dir_output 'temp')
mkdir -p "${DIR_OUTPUT}"
temp $fst_run $lst_run
