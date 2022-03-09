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

    file_result="$dir_result/md_parameter.tsv"

    while read -r task
    do
        # get output_param.dat at run01:
        
        file_param="${task}/calculation/run02/output_param.dat"
        header="$(awk '{print $1}' "${file_param}" | paste -s)"
        params="$(awk '{print $2}' "${file_param}" | paste -s)"
        is_first_task "$project_name" "$task" && echo -e "task_name\t$header"
        echo -e "${task}\t${params}"

    done < <(gen_task_list "$project_name") > "$file_result"
done < "${FILE_TARGET_PROJECTS}"
