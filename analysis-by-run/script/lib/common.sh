#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR


readonly DIR_ROOT="$(cd $(dirname $0)/../../..; pwd)"
readonly DIR_ANALYSIS_SERVICE="$(cd $(dirname $0)/../..; pwd)"
readonly DIR_ANALYSIS_BY_RUN="$(cd $(dirname $0)/..; pwd)"

readonly DIR_MD_SERVICE="${DIR_ROOT}/md-service"
readonly DIR_CALCULATION="${DIR_ROOT}/calculation"

readonly DIR_MD_SERVICE_OUTPUT="${DIR_MD_SERVICE}/output"
readonly DIR_PROJECT_PATHS="${DIR_MD_SERVICE_OUTPUT}/project-paths"

readonly FILE_TASK_SETTING="${DIR_ANALYSIS_SERVICE}/setting/target_projects.tsv"
readonly FILE_HOSTS="${DIR_ANALYSIS_SERVICE}/setting/excution_hosts.tsv"

readonly DIR_TASK="${DIR_ANALYSIS_BY_RUN}/task"
readonly DIR_PACKAGE="${DIR_ANALYSIS_BY_RUN}/package"

readonly FILE_ALL_TASK="${DIR_TASK}/task_all.txt"

export DIR_TASK FILENAME_EXEC_SCRIPT

parallel_execution() {
    set -eu
    local host_name="${1}"
    local num_core="${2}"
    file_task="${DIR_TASK}/task_${host_name}.txt"
    file_exec_script="script/${FILENAME_EXEC_SCRIPT}"
    ssh "${host_name}" << EOF
    cat "${file_task}" | \
        xargs -I{} -P${num_core} bash {}/"${file_exec_script}"
EOF
}

export -f parallel_execution