#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR
. "$(dirname $0)/lib/common.sh"

readonly FILE_ANA_SCRIPT=$(file_ana_script gr.sh)

export FILE_ANA_SCRIPT
echo "##### 解析の実行 #####" && {
    tail -n +2 "${FILE_HOSTS:?}" |
        xargs -I{} -P0 -t bash -c "parallel_excecution {} ${FILE_ANA_SCRIPT}" 
}
