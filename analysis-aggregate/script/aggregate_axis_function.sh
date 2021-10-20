#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR
. "$(dirname $0)/lib/common.sh"


readonly FILE_RESULT="${DIR_OUTPUT}/aggregate_axis_function.tsv"


while read filename_project_struct fst_run lst_run 
# use only filename_project_struct
do
    file_project_paths="${DIR_PROJECT_PATHS}/${filename_project_struct}"
    while read task
    do
        # get output_param.dat at run01
        dir_run01="${task}/calculation/run$(printf '%02d' $fst_run)"
        file_param="${dir_run01}/output_param.dat"
        header="$(awk '{print $1}' ${file_param} | paste -s)"
        params="$(awk '{print $2}' ${file_param} | paste -s)"
        [ "${task}" == "$(head -n 1 ${file_project_paths})" ] \
            && echo -e "taskname\t${header}"
        echo -e "${task}\t${params}"

    done < "${file_project_paths}"
done < <(tail -n +2 "${FILE_TASK_SETTING}") | tee "${FILE_RESULT}"
