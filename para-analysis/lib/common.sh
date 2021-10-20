#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR


readonly DIR_ROOT="$(cd $(dirname $0)/../../..; pwd)"
readonly DIR_ANALYSIS_SERVICE="$(cd $(dirname $0)/../..; pwd)"
readonly DIR_PARA_ANALYSIS="$(cd $(dirname $0)/..; pwd)"
readonly DIR_LIB="$(cd $(dirname $0); pwd)/lib"

readonly DIR_CALCULATION="${DIR_ROOT}/calculation"

readonly DIR_ANALYSIS1RUN="${DIR_ANALYSIS_SERVICE}/analysis-1run"
readonly DIR_ANALYSISALLRUN="${DIR_ANALYSIS_SERVICE}/analysis-allrun"

readonly FILE_TASK_SETTING="${DIR_ANALYSIS_SERVICE}/setting/target_projects.tsv"
readonly FILE_HOSTS="${DIR_ANALYSIS_SERVICE}/setting/excution_hosts.tsv"

readonly DIR_TASK="${DIR_PARA_ANALYSIS}/task"
readonly DIR_PACKAGE="${DIR_PARA_ANALYSIS}/package"

readonly FILE_TASK_ALL="${DIR_TASK}/task_all.txt"

export DIR_TASK FILENAME_EXEC_SCRIPT