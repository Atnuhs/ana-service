#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

. "$(dirname $0)/lib/common.sh"

readonly FST_RUN=$1
readonly LST_RUN=$2
readonly DIR_OUTPUT=$(dir_output thcd)
readonly FILE_EXE=$(file_exe thcd.out)

ana_allrun "${FILE_EXE}" "${DIR_OUTPUT}" "${FST_RUN}" "${LST_RUN}"

cat "${DIR_OUTPUT}/integ_ej_mean_lm.txt" | split_file '10' > "${DIR_OUTPUT}/integ_ej_mean_lm_sp.txt" 
