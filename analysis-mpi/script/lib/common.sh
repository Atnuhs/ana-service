#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly DIR_ROOT="$(cd "$(dirname "$0")/.."; pwd)"
readonly DIR_BUILD="${DIR_ROOT}/build"
readonly DIR_PROJECT_PATHS="/hdd1/abe/non-trial/md-service/output/project-paths"


split_file() {
    sp=$1 # 何行おきにの出力か
    awk "NR%${sp}==1"
}
