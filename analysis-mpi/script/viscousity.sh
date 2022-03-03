#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

while read -r task
do
    mkdir -p "$task/Analysis/viscousity" && cd "$task/Analysis"
    "${DIR_ROOT}/build/src/viscousity.out" <<< "$FST_CALC $LST_CALC"
    head -n "$LST_CALC" "$task/Analysis/GK_viscousity/integ.txt" | split_file 100 > "$task/Analysis/viscousity/integ.txt"
done < <(gen_task_list "${NAME_TARGET_PROJECT}")
