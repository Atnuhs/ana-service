#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

while read -r task fst_run lst_run fst_calc lst_calc
do
    init_dir_result "$task" 'rdc'
    cd "$task/Analysis"
    echo "${BASH_SOURCE[0]}: $(pwd): $fst_run $lst_run $fst_calc $lst_calc"
    mpirun -n "$NUM_PARA" "${DIR_ROOT}/build/src/rdc.out" <<< "$fst_run $lst_run $fst_calc $lst_calc" > /dev/null
    head -n "$lst_calc" "$task/Analysis/rdc/rmsd_mean.txt" | split_file 10 > "$task/Analysis/rdc/rmsd_calc_range.txt"
done < <(gen_task_and_run_and_rmsd_range)