#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR


readonly NAME_TARGET_PROJECT="$1"
readonly FST_RUN="$2"
readonly LST_RUN="$3"
readonly NUM_PARA="$4"

DIR_ROOT="$(cd "$(dirname "$0")/.."; pwd)"
readonly DIR_ROOT
readonly DIR_PROJECT_PATHS="/hdd1/abe/non-trial/md-service/output/project-paths"

gen_task_list () {
    local -r name_target_project="$1"
    cat "${DIR_PROJECT_PATHS}/${name_target_project}.txt"
}

while read -r task
do
    mkdir -p "$task/Analysis" && cd "$task/Analysis"
    mpirun -n "${NUM_PARA}" "${DIR_ROOT}/build/src/rmsd.out" <<< "$FST_RUN $LST_RUN"
done < <(gen_task_list "${NAME_TARGET_PROJECT}")