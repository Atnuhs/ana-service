#!/bin/bash

readonly DIR_SCRIPT_ROOT="$(dirname $(readlink -f $0))"

readonly FILE_HOSTS="${DIR_SCRIPT_ROOT:?}/../setting/excution_hosts.tsv"
readonly FILE_ALL_TASK="${DIR_SCRIPT_ROOT:?}/task_all.txt"
readonly TASK_NUM="$(cat "${FILE_ALL_TASK:?}" | wc -l)"
readonly ALL_CORE="$(awk '{sum+=$2} END{print sum;}' "${FILE_HOSTS:?}")"


echo "The number of core you can use is >> ${ALL_CORE:?} <<"
echo "all task :: ${TASK_NUM:?} => ${FILE_ALL_TASK##*/}"

readed_line=0
while read host_name core_num
do  
    host_task_num="$(((core_num*TASK_NUM)/ALL_CORE))"
    file_result="${DIR_SCRIPT_ROOT}/task_${host_name:?}.txt"
    sed -n "$((readed_line+1))","$((readed_line+host_task_num))"p "${FILE_ALL_TASK:?}" >${file_result:?}
    echo "$host_name :: $(cat ${file_result:?} | wc -l) => ${file_result##*/}"
    readed_line=$((readed_line+host_task_num))
done < <(head -n -1 "${FILE_HOSTS:?}" | tail -n +2)


last_host_name=$(tail -n 1 "${FILE_HOSTS:?}" | awk '{print $1}')
file_result="${DIR_SCRIPT_ROOT}/task_${last_host_name:?}.txt"
tail -n +$((readed_line+1)) "${FILE_ALL_TASK:?}" >${file_result:?}
echo "$last_host_name :: $(cat ${file_result:?} | wc -l) => ${file_result##*/}"
