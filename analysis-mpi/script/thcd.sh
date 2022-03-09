#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

while read -r task
do
    rm -rf "$task/Analysis/thcd"
    mkdir -p "$task/Analysis/thcd" && cd "$task/Analysis"
    pwd
    "${DIR_ROOT}/build/src/thcd.out" <<< "$FST_CALC $LST_CALC" > /dev/null
    head -n "$LST_CALC" "$task/Analysis/GK_thcd/integ.txt" | split_file 100 > "$task/Analysis/thcd/integ.txt"
done < <(gen_task_list)
