#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

. "$(dirname "$0")/lib/common.sh"

readonly NAME_TARGET_PROJECT="$1"
readonly FST_CALC="$2"
readonly LST_CALC="$3"

gen_task_list () {
    local -r name_target_project="$1"
    cat "${DIR_PROJECT_PATHS}/${name_target_project}.txt"
}

while read -r task
do
    mkdir -p "$task/Analysis/viscousity" && cd "$task/Analysis"
    "${DIR_ROOT}/build/src/viscousity.out" <<< "$FST_CALC $LST_CALC"
done < <(gen_task_list "${NAME_TARGET_PROJECT}")
