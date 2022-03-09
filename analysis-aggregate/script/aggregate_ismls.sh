#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"


    while read -r project_name
    do
        dir_result="$DIR_ROOT/output/$project_name"
        mkdir -p "$dir_result"
        echo "$dir_result"
        file_result="$dir_result/GK_integ_viscousity.tsv"
        [ -f "$file_result" ] && rm "$file_result"

        while read -r task
        do
            rate="${task##*/}"
            file_iemls="${task}/Analysis/viscousity/integ.txt"

            if [ -f "${file_result}" ]; then
                header="$(echo -e "y_${rate}")"
                paste "${file_result}" <(awk -v header="${header}" '
                    BEGIN{OFS="\t"; print header}
                    {print $2}
                ' "${file_iemls}") > "${file_result}.tmp"
                
                mv "${file_result}.tmp" "${file_result}"

            else
                header="$(echo -e "x\ty_${rate}")"
                awk -v header="${header}" '
                    BEGIN{OFS="\t"; print header}
                    {print $1, $2}
                ' "${file_iemls}" >"${file_result}"
            fi

        done < <(gen_task_list "${project_name}")

            done < "${FILE_TARGET_PROJECTS}"
