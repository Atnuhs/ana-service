#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR


readonly DIR_ROOT="$(cd $(dirname $0)/../../..; pwd)"
readonly DIR_ANALYSIS_SERVICE="$(cd $(dirname $0)/../..; pwd)"
readonly DIR_PARA_ANALYSIS="$(cd $(dirname $0)/..; pwd)"
readonly DIR_LIB="$(cd $(dirname $0); pwd)/lib"

readonly DIR_CALCULATION="${DIR_ROOT}/calculation"

readonly DIR_ANALYSIS1RUN="${DIR_ANALYSIS_SERVICE}/analysis-1run"

readonly FILE_TASK_SETTING="${DIR_PARA_ANALYSIS}/setting/target_projects.tsv"
readonly FILE_HOSTS="${DIR_PARA_ANALYSIS}/setting/excution_hosts.tsv"

readonly DIR_TASK="${DIR_PARA_ANALYSIS}/task"
readonly DIR_PACKAGE="${DIR_PARA_ANALYSIS}/package"

readonly FILE_ALL_TASK="${DIR_TASK}/task_all.txt"

export DIR_TASK FILENAME_EXEC_SCRIPT

mkdir -p "${DIR_TASK}"

parallel_excecution() {
    set -eu

    local host_name=$1
    local num_core=$2
    local file_excec_script=$3

    file_task="${DIR_TASK}/task_${host_name}.txt"
    ssh "${host_name}" << EOF
        cat ${file_task} |
            xargs -I {} -P${num_core} -t bash -c "cd {} && ${file_excec_script}"
EOF
}


file_ana_script() {
    local filename_ana_script=$1
    echo "${DIR_ANALYSIS1RUN}/script/${filename_ana_script}"
}

export -f parallel_excecution file_ana_script
export DIR_LIB 