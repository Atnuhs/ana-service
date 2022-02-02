#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

. "$(dirname "$0")/lib/common.sh"

readonly NAME_TARGET_PROJECT="$1"
readonly FST_RUN="$2"
readonly LST_RUN="$3"
readonly NUM_PARA="$4"


gen_task_list () {
    local -r name_target_project="$1"
    cat "${DIR_PROJECT_PATHS}/${name_target_project}.txt"
}

while read -r task
do
    mkdir -p "$task/Analysis/GK_viscousity" && cd "$task/Analysis"
    mpirun -n "${NUM_PARA}" "${DIR_ROOT}/build/src/GK_viscousity.out" <<< "$FST_RUN $LST_RUN"
done < <(gen_task_list "${NAME_TARGET_PROJECT}")
