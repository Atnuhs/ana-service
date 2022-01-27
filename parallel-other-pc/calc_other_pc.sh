#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

DIR_ROOT="$(cd $(dirname "$0")/../../; pwd)"

readonly HOSTNAME_REMOTE="$1"
readonly DIR_TARGET_CALC_ROOT="$2"
readonly RUNNUM="$3"
readonly SUBDIR_RUN="$(printf "calculation/run%02g" "${RUNNUM}")"

echo "${SUBDIR_RUN}"
# TMPDIR=$(ssh -n "${HOSTNAME_REMOTE}" "mktemp -d")

# rsync -ahvz "${DIR_TARGET_CALC}" "${HOSTNAME_REMOTE}:${TMPDIR}"
# rsync -ahvz "${DIR_TARGET_INPUT}" "${HOSTNAME_REMOTE}:${TMPDIR}"

# ssh -n "${HOSTNAME_REMOTE}" "rm -rf ${TMPDIR}"