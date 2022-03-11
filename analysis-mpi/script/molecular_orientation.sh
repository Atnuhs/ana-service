#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

while read -r task fst_run lst_run
do
    init_dir_result "$task" 'molecular_orientation'
    cd "$task/Analysis"
    pwd
    mpirun -n "${NUM_PARA}" "${DIR_ROOT}/build/src/molecular_orientation.out" <<< "$fst_run $fst_run" 
done < <(gen_task_and_run_range)