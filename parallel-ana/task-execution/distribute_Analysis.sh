#!/bin/bash


readonly DIR_SCRIPT_ROOT="$(cd "$(dirname $0)"&&pwd)"
readonly DIR_SERVICE_ROOT="$(cd "$(dirname $0)/../../"&&pwd)"

readonly DIR_RUN_ANA="${DIR_SERVICE_ROOT:?}/run-ana"
readonly DIR_DIST="${DIR_RUN_ANA:?}/dist"
readonly FILE_EXE_BUILD="${DIR_RUN_ANA:?}/cmake_build.sh"
readonly FILE_MAKE_DIST="${DIR_RUN_ANA:?}/make_dist.sh"
readonly FILE_ALL_TASK="${DIR_SCRIPT_ROOT:?}/../task-generate/task_all.txt"



echo "##### 解析用Fortranのコンパイル" && {
"${FILE_EXE_BUILD:?}"
"${FILE_MAKE_DIST:?}"
}


echo "##### 解析スクリプトの配布 #####"
while read dir_analysis
do
    echo "${dir_analysis:?} => $([ -d "${dir_analysis:?}" ] && echo 'T' || echo 'F')"
    mkdir -p "${dir_analysis:?}"/{exe,script}
    rsync -ah --delete "${DIR_DIST}/" "${dir_analysis:?}"
done < "${FILE_ALL_TASK:?}"
