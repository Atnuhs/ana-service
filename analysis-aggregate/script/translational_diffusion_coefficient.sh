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

    file_result="$dir_result/translational_diffusion_coefficient.tsv"

    while read -r task
    do
        dir_analysis="${task}/Analysis"
        file_thcd="${dir_analysis}/msd/tdc.txt"

        tdc=$(sed -n 1P "${file_thcd}" | awk '{print $1}')
        tdc_sd=$(sed -n 1P "${file_thcd}" | awk '{print $2}')
        is_first_task "$project_name" "$task" && echo -e "task_name\ttdc\ttdc_sd" 
        echo -e "${task}\t${tdc}\t${tdc_sd}"

    done < <(gen_task_list "$project_name") > "$file_result"
done < "${FILE_TARGET_PROJECTS}" 
