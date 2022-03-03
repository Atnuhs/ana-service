#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/../../setting/.env"

DIR_ROOT="$(cd "$(dirname "$0")/.."; pwd)"

# .envファイルで設定されている変数に読み取りのみの属性を付加
readonly DIR_PROJECT_PATHS NAME_TARGET_PROJECT
readonly FST_RUN LST_RAN
readonly FST_CALC LST_CALC
readonly NUM_PARA

gen_task_list () {
    cat "${DIR_PROJECT_PATHS}/${NAME_TARGET_PROJECT}.txt"
}

split_file() {
    sp=$1 # 何行おきにの出力か
    awk "NR%${sp}==1"
}