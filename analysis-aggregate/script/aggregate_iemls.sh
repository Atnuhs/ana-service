#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR
. "$(dirname $0)/lib/common.sh"

readonly DIR_RESULT="$DIR_ROOT/output/$NAME_TARGET_PROJECT"
mkdir -p "$DIR_RESULT"
readonly FILE_RESULT="$DIR_RESULT/GK_integ_thcd.tsv"

[ -f "$FILE_RESULT" ] && rm "$FILE_RESULT"

    while read -r task
    do
        rate="${task##*/}"
        file_iemls="${task}/Analysis/thcd/integ.txt"

        if [ -f "${FILE_RESULT}" ]; then
            header="$(echo -e "y_${rate}")"
            paste "${FILE_RESULT}" <(awk -v header="${header}" '
                BEGIN{OFS="\t"; print header}
                {print $2}
            ' "${file_iemls}") > "${FILE_RESULT}.tmp"
            
            mv "${FILE_RESULT}.tmp" "${FILE_RESULT}"

        else
            header="$(echo -e "x\ty_${rate}")"
            awk -v header="${header}" '
                BEGIN{OFS="\t"; print header}
                {print $1, $2}
            ' "${file_iemls}" >"${FILE_RESULT}"
        fi
    done < <(gen_task_list)
