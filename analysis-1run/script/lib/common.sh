#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly DIR_ROOT="$(cd $(dirname $0)/..; pwd)"
readonly DIR_WDIR="$(pwd)"

readonly DIR_SCRIPT="${DIR_ROOT}/script"
readonly DIR_BUILD="${DIR_ROOT}/build"
readonly DIR_SRC="${DIR_ROOT}/src"
readonly DIR_LIB="${DIR_ROOT}/lib"

readonly DIR_EXE="${DIR_BUILD}/src"

ana () {
    local filename_exe=$1 dirname_output=$2
    local -r file_exe="${DIR_EXE}/${filename_exe}"
    local -r dir_output="${DIR_WDIR}/${dirname_output}"

    [ -d "${dir_output}" ] && rm -r "${dir_output}"
    mkdir -p "${dir_output}"
    "${file_exe}"

    if [ -f './gmon.out' ];  then
        gprof "${file_exe}" './gmon.out' > "${dir_output}/gmon.log"
        echo "プロファイル結果 => ${dir_output}/gmon.log"
        rm './gmon.out'
    fi
}
