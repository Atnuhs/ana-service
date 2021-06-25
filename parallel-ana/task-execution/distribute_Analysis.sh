#!/bin/bash


readonly DIR_SCRIPT_ROOT="$(cd "$(dirname $0)"&&pwd)"
readonly DIR_SERVICE_ROOT="$(cd "$(dirname $0)/../../"&&pwd)"

readonly DIR_RUN_ANA="${DIR_SERVICE_ROOT:?}/run-ana"
readonly DIR_EXE="${DIR_RUN_ANA:?}/exe"
readonly DIR_EXEC_SCRIPT="${DIR_RUN_ANA:?}/exec-script"
readonly FILE_EXE_BUILD="${DIR_RUN_ANA:?}/cmake_build.sh"
readonly FILE_ALL_TASK="${DIR_SCRIPT_ROOT:?}/../task-generate/list_analysis.txt"



echo "##### 解析用Fortranのコンパイル" && {

bash "${FILE_EXE_BUILD:?}"
chmod 764 "${DIR_EXEC_SCRIPT:?}"/*.sh

}


echo "##### 解析スクリプトの配布 #####"
while read dir_analysis
do
    echo "${dir_analysis:?} => $([ -d "${dir_analysis:?}" ] && echo 'T' || echo 'F')"
    mkdir -p "${dir_analysis:?}"/{exe,script}
    rsync -ah --delete "${DIR_EXE:?}"/ "${dir_analysis:?}/exe"
    rsync -ah --delete "${DIR_EXEC_SCRIPT:?}"/ "${dir_analysis:?}/script"
done < "${FILE_ALL_TASK:?}"
