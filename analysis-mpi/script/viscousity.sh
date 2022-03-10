#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

while read -r task gk_vis_fst_calc gk_vis_lst_calc
do
    init_dir_result "$task" 'viscousity'
    cd "$task/Analysis"
    pwd
    "${DIR_ROOT}/build/src/viscousity.out" <<< "$gk_vis_fst_calc $gk_vis_lst_calc" > /dev/null
    head -n "$gk_vis_lst_calc" "$task/Analysis/GK_viscousity/integ.txt" | split_file 100 > "$task/Analysis/viscousity/integ.txt"
done < <(gen_task_and_gk_vis_range)
