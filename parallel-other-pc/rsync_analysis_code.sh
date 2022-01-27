#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR


DIR_ANA_PACKAGE="$(cd "$(dirname "$0")/.."; pwd)"
readonly DIR_ANA_PACKAGE
readonly FILE_REMOTE_SETTING="${DIR_ANA_PACKAGE}/parallel-other-pc/setting/remote_setting.tsv"
readonly FILE_RSYNC_IGNORE="${DIR_ANA_PACKAGE}/parallel-other-pc/setting/rsync_ignore"
readonly DIR_RSYNC_TARGET="${DIR_ANA_PACKAGE}/analysis-1run"


while read -r hostname_remote dir_root
do
    echo "@@@@@@ ${hostname_remote} @@@@@@@@@@@@@@@@@@@@@@@@@@@@"
    echo 'dir_rootの作成' | {
        ssh -n "${hostname_remote}" "mkdir -p ${dir_root}"
    }
    echo '解析コードのコピー' | {
        rsync -ahvz --exclude-from="${FILE_RSYNC_IGNORE}" "${DIR_RSYNC_TARGET}" "${hostname_remote}:${dir_root}"
    }

    echo '解析コードのコンパイル' | {
        ssh -n "${hostname_remote}" "hostname; ${dir_root}/analysis-1run/script/build.sh"
    }
    
done < "${FILE_REMOTE_SETTING}"