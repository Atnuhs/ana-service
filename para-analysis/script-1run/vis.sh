#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly DIR_SCRIPT="$(cd $(dirname ${0}); pwd)"
readonly DIR_LIB="${DIR_SCRIPT}/lib"
readonly FILENAME_EXEC_SCRIPT='ana_vis.sh' 

. "${DIR_LIB}/common.sh"

echo "##### 解析の実行 #####" : {
    tail -n +2 "${FILE_HOSTS:?}" | \
        xargs -I{} -P0 -t bash -c "parallel_execution {}" \
}
