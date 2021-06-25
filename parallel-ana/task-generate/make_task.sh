#!/bin/bash

readonly DIR_SCRIPT_ROOT="$(cd "$(dirname $0)"; pwd)"
readonly DIR_PROJECT_ROOT="$(cd "$(dirname $0)/../../../"; pwd)"

readonly FILE_TASK_SETTING="${DIR_SCRIPT_ROOT:?}/../setting/task_name_and_run_num.tsv"
readonly FILE_RESULT="${DIR_SCRIPT_ROOT:?}/list_analysis.txt"
readonly DIR_PROJECT_STRUCT="${DIR_PROJECT_ROOT:?}/project-struct"
readonly DIR_CALCULATION="${DIR_PROJECT_ROOT:?}/calculation"


while read filename_project_struct fst_run lst_run
do
    readonly FILE_PROJECT_STRUCT="${DIR_PROJECT_STRUCT:?}/${filename_project_struct:?}"
    while read task
    do
        for run in $(seq -f "%02g" "${fst_run:?}" "${lst_run:?}")
        do
            dir_analysis="${DIR_CALCULATION:?}/${task:?}/run${run:?}/Analysis"
            echo $dir_analysis
        done
    done < "${FILE_PROJECT_STRUCT:?}"
done < <(tail -n +2 "${FILE_TASK_SETTING:?}") | tee "${FILE_RESULT:?}"
