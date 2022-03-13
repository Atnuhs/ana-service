#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

echo "### $0"

while read -r project_name
do
    dir_result="$(dir_result "$project_name")"
    mkdir -p "$dir_result"
    echo "$dir_result"
    file_result="$dir_result/gr_3d.tsv"

    while read -r task
    do
        rate="${task##*/}"
        file_gr="${task}/Analysis/ext_gr/gr_3d.txt"

        if is_first_task "$project_name" "$task"; then
            header="$(echo -e "x\ty\tz_${rate}")"
            awk -v header="${header}" '
                BEGIN{OFS="\t"; print header}
                {print $1, $2, $3}
            ' "${file_gr}" >"${file_result}"

        else
            header="$(echo -e "z_${rate}")"
            paste "${file_result}" <(awk -v header="${header}" '
                BEGIN{OFS="\t"; print header}
                {print $3}
            ' "${file_gr}") > "${file_result}.tmp"
            
            mv "${file_result}.tmp" "${file_result}"
         fi
         
    done < <(gen_task_list "$project_name")
done < <( tail -n +2 "${FILE_TARGET_PROJECTS}" | awk '{print $1}')
