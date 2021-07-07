#!/bin/bash

readonly DIR_ANALYSIS_SERVICE="$(cd "$(dirname $0)/.."; pwd)"
readonly DIR_NON_TRIAL="$(cd "$(dirname $0)/../.."; pwd)"
readonly DIR_AGGREGATE="$(cd "$(dirname $0)"; pwd)"

readonly FILE_TASK_SETTING="${DIR_ANALYSIS_SERVICE:?}/setting/target_projects.tsv"
readonly FILE_RESULT="${DIR_AGGREGATE:?}/result.tsv"
readonly DIR_PROJECT_PATHS="${DIR_NON_TRIAL:?}/project-paths"
readonly DIR_CALCULATION="${DIR_NON_TRIAL:?}/calculation"


while read filename_project_struct fst_run lst_run
do
    readonly FILE_PROJECT_PATHS="${DIR_PROJECT_PATHS:?}/${filename_project_struct:?}"
    while read task
    do
        dir_analysis="${task:?}/Analysis"
        file_thcd="${dir_analysis:?}/thcd/thcd.txt"
        file_temp="${dir_analysis:?}/temp/temp_mean.txt"
        # TODO　別の参照方法を考える
        rate="${task##*/}"
        temp=$(sed -n 1P ${file_temp:?} | awk '{print $2}')
        thcd=$(sed -n 2P ${file_thcd:?} | awk '{print $1}')
        thcd_se=$(sed -n 2P ${file_thcd:?} | awk '{print $2}')
        echo -e "${task}\t${rate}\t${thcd}\t${thcd_se}\t${temp}"

    done < "${FILE_PROJECT_PATHS:?}"
done < <(tail -n +2 "${FILE_TASK_SETTING:?}") | tee "${FILE_RESULT:?}"
