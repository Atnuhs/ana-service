#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR
. "$(dirname $0)/lib/common_allrun.sh"

file_project_paths () {
    local -r dir_project_paths="${DIR_ROOT}/md-service/output/project-paths"
    local -r filename_project_paths=$1
    echo "${dir_project_paths}/${filename_project_paths}"
}


while read filename_project_struct fst_run lst_run
do
    while read task
    do
        echo -e "${task:?}/Analysis\t${fst_run}\t${lst_run}"
    done < "$(file_project_paths ${filename_project_struct})"
done < <(tail -n +2 "${FILE_TASK_SETTING}") | tee "${FILE_TASK_ALL}"

