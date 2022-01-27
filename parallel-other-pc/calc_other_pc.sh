#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

DIR_ROOT="$(cd $(dirname "$0")/../../; pwd)"

readonly HOSTNAME_REMOTE="$1"
readonly DIR_TARGET_CALC="$2"

echo "${DIR_ROOT}"
TMPDIR=$(ssh -n "${HOSTNAME_REMOTE}" "mktemp -d")


rsync -ahvz "${DIR_TARGET_CALC}" "${HOSTNAME_REMOTE}:${TMPDIR}"

ssh -n "${HOSTNAME_REMOTE}" "rm -rvf ${TMPDIR}"