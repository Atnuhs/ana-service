#!/bin/bash


readonly DIR_ROOT="$(cd "$(dirname $0)/../../.."&&pwd)"
readonly DIR_ANALYSIS_SERVICE="$(cd "$(dirname $0)/../.."&&pwd)"
readonly DIR_ANALYSIS_BY_RUN="$(cd "$(dirname $0)/.."&&pwd)"

readonly DIR_PACKAGE="${DIR_ANALYSIS_BY_RUN:?}/package"
readonly DIR_TASK="${DIR_ANALYSIS_SERVECE:?}/task"
readonly FILE_ALL_TASK="${DIR_ANALYSIS_BY_RUN:?}/../task-generate/task_all.txt"



echo "##### 解析用Fortranのコンパイル" && {
"${DIR_PACKAGE:?}/cmake_build.sh"
"${DIR_PACKAGE:?}/make_dist.sh"
}


echo "##### 解析スクリプトの配布 #####"
while read dir_analysis
do
    echo "${dir_analysis:?} => $([ -d "${dir_analysis:?}" ] && echo 'T' || echo 'F')"
    mkdir -p "${dir_analysis:?}"/{exe,script}
    rsync -ah --delete "${DIR_PACKAGE}/dist/" "${dir_analysis:?}"
done < "${FILE_ALL_TASK:?}"
