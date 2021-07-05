#!/bin/bash

readonly DIR_ANALYSIS_SERVICE="$(cd "$(dirname $0)/../../.."; pwd)"
readonly DIR_NON_TRIAL="$(cd "$(dirname $0)/../../../.."; pwd)"
readonly DIR_TASK="$(cd "$(dirname $0)/../"; pwd)"

readonly FILE_TASK_SETTING="${DIR_ANALYSIS_SERVICE:?}/setting/target_projects.tsv"
readonly FILE_RESULT="${DIR_TASK:?}/task_all.txt"
readonly DIR_PROJECT_PATHS="${DIR_NON_TRIAL:?}/project-paths"
readonly DIR_CALCULATION="${DIR_NON_TRIAL:?}/calculation"


while read filename_project_struct fst_run lst_run
do
    readonly FILE_PROJECT_PATHS="${DIR_PROJECT_PATHS:?}/${filename_project_struct:?}"
    while read task
    do
        echo "${task:?}/Analysis ${fst_run} ${lst_run}"  
    done < "${FILE_PROJECT_PATHS:?}"

done < <(tail -n +2 "${FILE_TASK_SETTING:?}") | tee "${FILE_RESULT:?}"
