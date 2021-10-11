#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

. "$(dirname $0)/common.sh"

readonly FILE_TASK=$1
readonly NUMCORE=$2
readonly FILE_ANA_SCRIPT=$3

cat "${FILE_TASK}" |
    xargs -I{} -P${NUMCORE} -t bash {}/"${FILE_ANA_SCRIPT}"
    