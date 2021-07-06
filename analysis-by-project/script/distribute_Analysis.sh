#!/bin/bash


readonly DIR_NON_TRIAL="$(cd "$(dirname $0)/../../.."&&pwd)"
readonly DIR_ANALYSIS_SERVICE="$(cd "$(dirname $0)/../.."&&pwd)"
readonly DIR_ANALYSIS_BY_PROJECT="$(cd "$(dirname $0)/.."&&pwd)"

readonly DIR_PACKAGE="${DIR_ANALYSIS_BY_PROJECT:?}/package"
readonly DIR_TASK="${DIR_ANALYSIS_BY_PROJECT:?}/task"
readonly FILE_ALL_TASK="${DIR_TASK:?}/task_all.tsv"



echo "##### 解析用Fortranのコンパイル" && {
"${DIR_PACKAGE:?}/cmake_build.sh"
"${DIR_PACKAGE:?}/make_dist.sh"
}


echo "##### 解析スクリプトの配布 #####"
while read dir_analysis fst_run lst_run
do
    echo "${dir_analysis:?} => $([ -d "${dir_analysis:?}" ] && echo 'T' || echo 'F')"
    mkdir -p "${dir_analysis:?}"/{exe,script}
    rsync -ah --delete "${DIR_PACKAGE}/dist/" "${dir_analysis:?}"
    echo "${fst_run:?} ${lst_run:?}" > "${dir_analysis:?}/analysis_run.txt"
done < "${FILE_ALL_TASK:?}"
