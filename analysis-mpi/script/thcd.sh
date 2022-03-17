#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

while read -r task fst_calc lst_calc
do
    init_dir_result "$task" 'thcd'
    cd "$task/Analysis"
    echo "${BASH_SOURCE[0]}: $(pwd): $fst_calc $lst_calc"
    "${DIR_ROOT}/build/src/thcd.out" <<< "$fst_calc $lst_calc" > /dev/null
    head -n "$lst_calc" "$task/Analysis/GK_thcd/integ.txt" | split_file 100 > "$task/Analysis/thcd/integ.txt"
done < <(gen_task_and_gk_thcd_range)
