#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly FILENAME_EXE="thcd.out"
readonly DIRNAME_RESULT="thcd"

readonly DIR_SCRIPT="$(cd "$(dirname "${0}")"; pwd)"
readonly DIR_LIB="${DIR_SCRIPT}/lib"

. "${DIR_LIB}/common.sh"


read fst_run lst_run < "${FILE_ANALYSIS_RUN:?}"
echo "##### dir => ${DIR_ANALYSIS:?}"
mkdir -p "${DIR_RESULT:?}"

cd "${DIR_ANALYSIS:?}"
"${FILE_EXE:?}" <<<"${fst_run:?} ${lst_run:?}"

cat "${DIR_RESULT}/integ_ej_mean.txt" | split_file '100' > "${DIR_RESULT}/integ_ej_mean_sp.txt" 