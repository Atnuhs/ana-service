#!/bin/bash


readonly DIR_ANALYSIS_SERVICE="$(cd "$(dirname $0)/../../.."; pwd)"
readonly FILE_HOSTS="${DIR_ANALYSIS_SERVICE:?}/setting/excution_hosts.tsv"

readonly DIR_TASK="$(cd "$(dirname $0)/../"; pwd)"
readonly FILE_ALL_TASK="${DIR_TASK:?}/task_all.tsv"

readonly TASK_NUM="$(cat "${FILE_ALL_TASK:?}" | wc -l)"
readonly ALL_CORE="$(awk '{sum+=$2} END{print sum;}' "${FILE_HOSTS:?}")"


echo "The number of core you can use is >> ${ALL_CORE:?} <<"
echo "all task :: ${TASK_NUM:?} => ${FILE_ALL_TASK##*/}"

readed_line=0
while read host_name core_num
do  
    host_task_num="$(((core_num*TASK_NUM)/ALL_CORE))"
    file_result="${DIR_TASK}/task_${host_name:?}.tsv"

    sed -n "$((readed_line+1))","$((readed_line+host_task_num))"p "${FILE_ALL_TASK:?}" >${file_result:?}
    echo "$host_name :: $(cat ${file_result:?} | wc -l) => ${file_result##*/}"
    readed_line=$((readed_line+host_task_num))
done < <(head -n -1 "${FILE_HOSTS:?}" | tail -n +2)


last_host_name=$(tail -n 1 "${FILE_HOSTS:?}" | awk '{print $1}')
file_result="${DIR_TASK}/task_${last_host_name:?}.tsv"
tail -n +$((readed_line+1)) "${FILE_ALL_TASK:?}" >${file_result:?}
echo "$last_host_name :: $(cat ${file_result:?} | wc -l) => ${file_result##*/}"