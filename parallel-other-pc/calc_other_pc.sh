#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

DIR_ROOT="$(cd $(dirname "$0")/../../; pwd)"

readonly HOSTNAME_REMOTE="$1"
readonly DIR_TARGET_CALC_ROOT="$2"
readonly RUNNUM="$3"
readonly FILENAME_ANALYSIS="$4"
readonly SUBDIR_RUN="$(printf "calculation/run%02g" "${RUNNUM}")"

TMPDIR=$(ssh -n "${HOSTNAME_REMOTE}" "mktemp -d")

: 計算ディレクトリコピー | {
    cd "$DIR_TARGET_CALC_ROOT"
    rsync -ahvzR "${SUBDIR_RUN}" "${HOSTNAME_REMOTE}:${TMPDIR}"
    rsync -ahvz "input" "${HOSTNAME_REMOTE}:${TMPDIR}"
}

: 解析 | {
    ssh -n "${HOSTNAME_REMOTE}" "cd ${TMPDIR}/${SUBDIR_RUN}; mkdir Analysis; cd Analysis; ls ../../../; "
}

ssh -n "${HOSTNAME_REMOTE}" "rm -rf ${TMPDIR}"