#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly DIR_SCRIPT="$(cd $(dirname ${0}); pwd)"
readonly DIR_LIB="${DIR_SCRIPT}/lib"

. "${DIR_LIB}/common.sh"
readonly FILE_RESULT="${DIR_OUTPUT}/aggregate_r_diff_coef.tsv"


while read filename_project_struct fst_run lst_run
do
    file_project_paths="${DIR_PROJECT_PATHS}/${filename_project_struct}"
    while read task
    do
        dir_analysis="${task}/Analysis"
        file_thcd="${dir_analysis}/rmsd/rdc_mean.txt"
        file_temp="${dir_analysis}/temp/temp_mean.txt"
        # TODO　別の参照方法を考える
        rate="${task##*/}"
        temp=$(sed -n 1P ${file_temp} | awk '{print $2}')
        rdc=$(sed -n 1P ${file_thcd} | awk '{print $1}')
        rdc_sd=$(sed -n 1P ${file_thcd} | awk '{print $2}')
        echo -e "${task}\t${rate}\t${rdc}\t${rdc_sd}\t${temp}"

    done < "${file_project_paths}"
done < <(tail -n +2 "${FILE_TASK_SETTING}") | tee "${FILE_RESULT}"
