#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

. "$(dirname $0)/../lib/common.sh"

mkdir -p "${DIR_TASK}"

export DIR_TASK FILENAME_EXEC_SCRIPT

parallel_execution() {
    set -eu
    host_name="${1}"
    num_core="${2}"
    filename_task="task_${host_name}.tsv"
    file_task="${DIR_TASK}/${filename_task}"
    tasks=$(cat "${file_task}" | awk '{print $1}')
    ssh "${host_name}" << EOF
    echo "${tasks}" | \
        xargs -I {} -P${num_core} bash {}/script/${FILENAME_EXEC_SCRIPT}
EOF
}



export -f parallel_execution