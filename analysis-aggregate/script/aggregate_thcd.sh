#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR
. "$(dirname $0)/lib/common.sh"

readonly FILE_RESULT="${DIR_OUTPUT}/aggregate_thcd.tsv"


while read filename_project_struct fst_run lst_run
do
    file_project_paths="${DIR_PROJECT_PATHS}/${filename_project_struct}"
    echo "${file_project_paths}"
    while read task
    do
        dir_analysis="${task}/Analysis"
        file_thcd="${dir_analysis}/thcd/thcd.txt"
        file_temp="${dir_analysis}/temp/temp_mean.txt"
        # TODO　別の参照方法を考える
        rate="${task##*/}"
        temp=$(sed -n 1P ${file_temp} | awk '{print $2}')
        thcd=$(sed -n 1P ${file_thcd} | awk '{print $1}')
        thcd_se=$(sed -n 1P ${file_thcd} | awk '{print $2}')
        echo -e "${task}\t${rate}\t${thcd}\t${thcd_se}\t${temp}"

    done < "${file_project_paths}"
done < <(tail -n +2 "${FILE_TASK_SETTING}") | tee "${FILE_RESULT}"
