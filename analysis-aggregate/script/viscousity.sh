#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

while read -r project_name 
do
    dir_result="$(dir_result "$project_name")"
    mkdir -p "$dir_result"
    echo "$dir_result"

    file_result="$dir_result/viscousity.tsv"

    while read -r task
    do
        file_viscousity="${task}/Analysis/viscousity/viscousity.txt"
        viscousity=$(sed -n 1P "${file_viscousity}" | awk '{print $1}')
        viscousity_se=$(sed -n 1P "${file_viscousity}" | awk '{print $2}')
        is_first_task "$project_name" "$task" && echo -e "task_name\tviscousity\tviscousity_se"
        echo -e "${task}\t${viscousity}\t${viscousity_se}"

    done < <(gen_task_list "$project_name") > "$file_result"
done < "${FILE_TARGET_PROJECTS}"
