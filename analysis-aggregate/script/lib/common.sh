#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/../../setting/.env"

DIR_ROOT="$(cd "$(dirname "$0")/.."; pwd)"
DIR_SETTING="${DIR_ROOT}/../setting"

FILE_TARGET_PROJECTS="${DIR_SETTING}/target_projects.tsv"

readonly DIR_ROOT DIR_SETTING
# .envファイルで設定されている変数に読み取りのみの属性を付加
readonly DIR_PROJECT_PATHS 

gen_task_list () {
    local project_name=$1
    cat "${DIR_PROJECT_PATHS}/${project_name}.txt"
}

dir_result () {
    local project_name=$1
    echo "${DIR_ROOT}/output/${project_name}"
}

is_first_task () {
    local project_name=$1
    local task=$2
    test "$task" == "$(gen_task_list "$project_name" | head -n 1)" 
}
 
split_file() {
    local sp=$1 # 何行おきにの出力か
    awk "NR%${sp}==1"
}