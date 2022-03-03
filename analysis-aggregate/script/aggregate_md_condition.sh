#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR
. "$(dirname $0)/lib/common.sh"

readonly FILE_RESULT="${DIR_OUTPUT}/aggregate_md_condition.tsv"

# TODO ほしい値を考える。 =========
# プロジェクトパス 変数名 レート run01step数 run02以降step数 時間刻み 温度  
# 変数名 ファイルパスから
# レート ファイルパスから
# run01step数 50000
# run02以降step数 condition_input.txt　1行目
# 時間刻み　condition_input.txt 4行目
# 設定温度 condition_input.txt 2行目
# 温度コントロール　run01 30000ステップまで。
# 運動量補正　5000ステップごと


while read filename_project_struct fst_run lst_run 
# use only filename_project_struct
do
    file_project_paths="${DIR_PROJECT_PATHS}/${filename_project_struct}"
    file_project_struct="${DIR_PROJECT_STRUCT}/${filename_project_struct}"
    echo -e "path\tvariable\trate\t\
run01のstep\tstep\trun数\ttemp0\tdt\t統計量"
    while read task
    do
        project_path="${task}"
        file_condition_input="${task}/input/condition_input.txt"
        file_current_run="${task}/current_run.txt"

        rate="$(basename $project_path)"
        variable=$(basename $(dirname $project_path))
        step_run01=50000

        pre_step="$(sed -n 1P ${file_condition_input})"
        step=${pre_step% ! nstep}
        pre_temp0="$(sed -n 2P ${file_condition_input})"
        temp0=${pre_temp0%!temp0}
        pre_dt="$(sed -n 4P ${file_condition_input})"
        ddt=${pre_dt%!dt}
        dt=${ddt//d/e}
        num_run="$(cat ${file_current_run})"
        echo $step
        stat_time=$(python -c "print($step*($num_run-1)*$dt)")
        echo -e "${project_path}\t${variable}\t${rate}\t${step_run01}\t${step}\t${num_run}\t${temp0}\t${dt}\t${stat_time}"

    done < "${file_project_paths}"
done < <(tail -n +2 "${FILE_TASK_SETTING}") | tee "${FILE_RESULT}"