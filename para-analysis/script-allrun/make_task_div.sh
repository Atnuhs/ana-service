#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR
. "$(dirname $0)/lib/common_allrun.sh"

all_task_num () { cat "${FILE_TASK_ALL}" | wc -l ;}
all_core_num () { awk '{sum+=$2} END{print sum;}' "${FILE_HOSTS}" ;}
file_task () { echo "${DIR_TASK}/task_${1}.tsv" ;}

is_last_host_name () {
    declare -r HOST_NAME=$1
    declare -r LAST_HOST_NAME="$(tail -n 1 "${FILE_HOSTS}" | awk '{print $1}')"
    [ "${HOST_NAME}" == "${LAST_HOST_NAME}" ]
}

part_task_num () {
    declare -r HOST_NAME=$1 CORE_NUM=$2
    declare -r ALL_TASK_NUM="$(all_task_num)"
    declare -r ALL_CORE_NUM="$(all_core_num)"
    echo $(( (CORE_NUM*ALL_TASK_NUM) / ALL_CORE_NUM ))
}

cut_task () {
    declare -r r_top=$1 r_bottom=$2
    sed -n "${r_top},${r_bottom}p" "${FILE_TASK_ALL}"
}

echo "使用可能なコア数 >> $(all_core_num) <<"
echo "all task :: $(all_task_num) => ${FILE_TASK_ALL##*/}"

readed_line=0
while read host_name core_num
do
    file_result="$(file_task ${host_name})"
    part_task_num="$(part_task_num ${host_name} ${core_num})"

    r_top="$(( readed_line+1 ))"
    r_bottom="$(( readed_line+part_task_num ))"
    is_last_host_name "${host_name}" && r_bottom="$(all_task_num)"
    part_task_num=$(( r_bottom - r_top + 1 ))

    (( part_task_num == 0 )) &&
        { : >${file_result} && continue ;}

    echo "[ ${host_name} ] :: ${r_top}-${r_bottom} (${part_task_num}) => ${file_result##*/}"
    cut_task "${r_top}" "${r_bottom}" | tee ${file_result}

    readed_line="${r_bottom}"
done < <(tail -n +2 "${FILE_HOSTS}")
