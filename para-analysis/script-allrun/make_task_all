#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly DIR_SCRIPT="$(cd $(dirname ${0}); pwd)"
readonly DIR_LIB="${DIR_SCRIPT}/lib"
readonly FILENAME_EXEC_SCRIPT='ana_gr.sh' 
. "${DIR_LIB}/common.sh"

while read filename_project_struct fst_run lst_run
do
    FILE_PROJECT_PATHS="${DIR_PROJECT_PATHS:?}/${filename_project_struct:?}"
    while read task
    do
        echo -e "${task:?}/Analysis\t${fst_run}\t${lst_run}"  
    done < "${FILE_PROJECT_PATHS:?}"

done < <(tail -n +2 "${FILE_TASK_SETTING:?}") | tee "${FILE_ALL_TASK:?}"
