#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

. "$(dirname $0)/lib/common.sh"

readonly DIR_OUTPUT=$(dir_output gr)
readonly FILE_EXE=$(file_exe gr.out)

read fst_run lst_run < "${FILE_ANALYSIS_RUN}"
echo "##### dir => ${DIR_WDIR}"
mkdir -p "${DIR_OUTPUT}"
"${FILE_EXE}" <<<"${fst_run} ${lst_run}"