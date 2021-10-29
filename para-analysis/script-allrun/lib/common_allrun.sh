#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

. "$(dirname $0)/../lib/common.sh"

mkdir -p "${DIR_TASK}"


parallel_machine() {
    local host_name=$1
    local num_core=$2
    local file_excec_script=$3

    local file_task="${DIR_TASK}/task_${host_name}.tsv"
    local num_task="$(cat ${file_task} | wc -l)"

    [[ ${num_task} == 0 ]] && return 0
    
    echo "${file_excec_script}"
    ssh -T "${host_name}" << EOF
        cat ${file_task} |
            xargs -L1 -P${num_core} bash -c "
                mkdir -p \\\$0
                cd \\\$0
                echo ${file_excec_script##*/}: \\\$0
                ${file_excec_script} \\\$1 \\\$2
            "
EOF
}

parallel_analysis() {
    local -r filename_ana_script=$1
    local -r file_ana_script=$(file_ana_script ${filename_ana_script})

    echo "##### 解析の実行 #####" && {
        tail -n +2 "${FILE_HOSTS}" |
            xargs -I{} -P0 bash -c "parallel_machine {} ${file_ana_script}"
    }

}


clean_analysis() {
    set -x
    local file_task="${DIR_TASK}/task_all.txt"
    while read path fst_run lst_run
    do
        [ -e "${path}" ] && rm -r "${path}"
    done < <(cat ${file_task})
}


file_ana_script() {
    local filename_ana_script=$1
    echo "${DIR_ANALYSISALLRUN}/script/${filename_ana_script}"
}

export -f parallel_machine