#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR


readonly DIR_ROOT="$(cd $(dirname $0)/../../..; pwd)"
readonly DIR_ANALYSIS_SERVICE="$(cd $(dirname $0)/../..; pwd)"
readonly DIR_AGGREGATE="$(cd $(dirname $0)/..; pwd)"
readonly DIR_SCRIPT="$(cd $(dirname $0); pwd)"
readonly DIR_LIB="${DIR_SCRIPT}/lib"

readonly DIR_MD_SERVICE="${DIR_ROOT}/md-service"
readonly DIR_CALCULATION="${DIR_ROOT}/calculation"

readonly DIR_MD_SERVICE_OUTPUT="${DIR_MD_SERVICE}/output"
readonly DIR_PROJECT_PATHS="${DIR_MD_SERVICE_OUTPUT}/project-paths"
readonly DIR_PROJECT_STRUCT="${DIR_MD_SERVICE_OUTPUT}/project-struct"

readonly DIR_OUTPUT="${DIR_AGGREGATE}/output"

readonly FILE_TASK_SETTING="${DIR_ANALYSIS_SERVICE}/setting/target_projects.tsv"
readonly FILE_HOSTS="${DIR_ANALYSIS_SERVICE}/setting/excution_hosts.tsv"

