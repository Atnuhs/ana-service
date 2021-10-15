#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly DIR_ROOT="$(cd $(dirname $0)/..; pwd)"
readonly DIR_WDIR="$(pwd)"
readonly DIR_CALCULATION=$(cd ../calculation ; pwd)

readonly DIR_SCRIPT="${DIR_ROOT}/script"
readonly DIR_BUILD="${DIR_ROOT}/build"
readonly DIR_SRC="${DIR_ROOT}/src"
readonly DIR_LIB="${DIR_ROOT}/lib"

readonly DIR_EXE="${DIR_BUILD}/src"

readonly FILE_STDDEV="${DIR_SCRIPT}/lib/stddev.awk"
readonly FILE_ANALYSIS_RUN="${DIR_WDIR}/analysis_run.txt"


file_exe () {
    local filename_exe=$1
    echo "${DIR_EXE}/${filename_exe}"
}

dir_output () {
    local dirname_output=$1
    echo "${DIR_WDIR}/${dirname_output}"
}

ana () {
    local -r file_exe=$1 dir_output=$2
    local fst_run lst_run
    
    read fst_run lst_run < "${FILE_ANALYSIS_RUN}"
    mkdir -p "${dir_output}"
    "${file_exe}" <<<"${fst_run} ${lst_run}"
}

split_file() {
    sp="${1}"
    awk "NR%${sp}==1"
}
