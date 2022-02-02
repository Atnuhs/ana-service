#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

"${DIR_ROOT}/script/temp.sh"
./gr.sh
./GK_thcd.sh
./GK_viscousity.sh
./thcd.sh
./viscousity.sh
./msd.sh
./rmsd.sh
