#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

while read -r task fst_run lst_run
do
    init_dir_result "$task" 'gr'
    cd "$task/Analysis"
    (( (lst_run>fst_run+NUM_PARA-1)&&(lst_run=fst_run+NUM_PARA-1) ))
    echo "${BASH_SOURCE[0]}: $(pwd): $fst_run $lst_run"
    mpirun -n "${NUM_PARA}" "${DIR_ROOT}/build/src/gr.out" <<< "$fst_run $lst_run" > /dev/null
done < <(gen_task_and_run_range)