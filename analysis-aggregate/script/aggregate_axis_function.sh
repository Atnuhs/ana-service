#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly DIR_SCRIPT="$(cd $(dirname ${0}); pwd)"
readonly DIR_LIB="${DIR_SCRIPT}/lib"

. "${DIR_LIB}/common.sh"
readonly FILE_RESULT="${DIR_AGGREGATE}/aggregate_axis_function.tsv"


while read filename_project_struct fst_run lst_run 
# use only filename_project_struct
do
    file_project_paths="${DIR_PROJECT_PATHS}/${filename_project_struct}"
    while read task
    do
        # get output_param.dat at run01
        dir_run01="${task}/calculation/run01"
        file_param="${dir_run01}/output_param.dat"
        echo "${task}\t$(paste -s $file_param)"

    done < "${file_project_paths}"
done < <(tail -n +2 "${FILE_TASK_SETTING}") | tee "${FILE_RESULT}"
