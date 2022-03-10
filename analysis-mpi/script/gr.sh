#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

while read -r task
do
    init_dir_result "$task" 'gr'
    cd "$task/Analysis"
    pwd
    mpirun -n "${NUM_PARA}" "${DIR_ROOT}/build/src/gr.out" <<< "$FST_RUN $LST_RUN" > /dev/null
done < <(gen_task_list)