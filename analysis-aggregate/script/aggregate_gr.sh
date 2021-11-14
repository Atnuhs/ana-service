#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR
. "$(dirname $0)/lib/common.sh"


readonly FILE_RESULT="${DIR_OUTPUT}/aggregate_gr.tsv"
[ -f "${FILE_RESULT}" ] && rm "${FILE_RESULT}"

while read filename_project_struct fst_run lst_run
do
    file_project_paths="${DIR_PROJECT_PATHS}/${filename_project_struct}"
    while read task
    do
        rate="${task##*/}"
        file_gr="${task}/Analysis/gr/gr_mean.txt"

        if [ -f "${FILE_RESULT}" ]; then
            header="$(echo -e "y_${rate}\tyse_${rate}")"
            paste "${FILE_RESULT}" <(awk -v header="${header}" '
                BEGIN{OFS="\t"; print header}
                {print $2, $3}
            ' "${file_gr}") > "${FILE_RESULT}.tmp"
            
            mv "${FILE_RESULT}.tmp" "${FILE_RESULT}"

        else
            header="$(echo -e "x\ty_${rate}\tyse_${rate}")"
            awk -v header="${header}" '
                BEGIN{OFS="\t"; print header}
                {print $1, $2, $3}
            ' "${file_gr}" >"${FILE_RESULT}"
        fi
    done < "${file_project_paths}"
done < <(tail -n +2 "${FILE_TASK_SETTING}")
