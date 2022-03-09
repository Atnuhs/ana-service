#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"


while read -r task
do
    mkdir -p "$task/Analysis/gr" && cd "$task/Analysis"
    mpirun -n "${NUM_PARA}" "${DIR_ROOT}/build/src/gr.out" <<< "$FST_RUN $LST_RUN"
done < <(gen_task_list)