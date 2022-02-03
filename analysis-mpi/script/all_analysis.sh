#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

"${DIR_ROOT}/script/temp.sh"
"${DIR_ROOT}/script/gr.sh"
"${DIR_ROOT}/script/GK_thcd.sh"
"${DIR_ROOT}/script/GK_viscousity.sh"
"${DIR_ROOT}/script/thcd.sh"
"${DIR_ROOT}/script/viscousity.sh"
"${DIR_ROOT}/script/msd.sh"
"${DIR_ROOT}/script/rmsd.sh"
