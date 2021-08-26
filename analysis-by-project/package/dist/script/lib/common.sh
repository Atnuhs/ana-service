#!/bin/bash

set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly DIR_ANALYSIS="$(cd "$(dirname "${0}")/.."; pwd)"
readonly DIR_RATE="$(cd "$(dirname "${0}")/../.."; pwd)"

readonly DIR_CALCULATION="${DIR_RATE}/calculation"

readonly FILE_EXE="${DIR_ANALYSIS}/bin/${FILENAME_EXE}"
readonly FILE_STDDEV="${DIR_ANALYSIS}/script/lib/stddev.awk"

readonly DIR_RESULT="${DIR_ANALYSIS}/${DIRNAME_RESULT}"

readonly FILE_ANALYSIS_RUN="${DIR_ANALYSIS}/analysis_run.txt"

split_file() {
    sp="${1}"
    awk "NR%${sp}==1"
}

export -f split_file
