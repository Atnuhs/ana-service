#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/../../setting/.env"

DIR_ROOT="$(cd "$(dirname "$0")/.."; pwd)"
readonly DIR_BUILD="${DIR_ROOT}/build"
readonly FILE_TARGET_PROJECTS="${DIR_ROOT}/../setting/target_projects.tsv"

# .envファイルで設定されている変数に読み取りのみの属性を付加
readonly DIR_PROJECT_PATHS NAME_TARGET_PROJECT
readonly NUM_PARA

gen_task_and_run_range () {
    while read -r line fst_run lst_run
    do
        xargs -I{} echo "{} $fst_run $lst_run" <"${DIR_PROJECT_PATHS}/${line}.txt"
    done < <( tail -n +2 "${FILE_TARGET_PROJECTS}" |
        awk '{print $1,$2,$3}' )
}

gen_task_and_gk_thcd_range () {
    while read -r line gk_thcd_fst_calc gk_thcd_lst_calc
    do
        xargs -I{} echo "{} $gk_thcd_fst_calc $gk_thcd_lst_calc" <"${DIR_PROJECT_PATHS}/${line}.txt"
    done < <( tail -n +2 "${FILE_TARGET_PROJECTS}" |
        awk '{print $1,$4,$5}')
}

gen_task_and_gk_vis_range () {
    while read -r line gk_vis_fst_calc gk_vis_lst_calc
    do
        xargs -I{} echo "{} $gk_vis_fst_calc $gk_vis_lst_calc" <"${DIR_PROJECT_PATHS}/${line}.txt"
    done < <( tail -n +2 "${FILE_TARGET_PROJECTS}" |
        awk '{print $1,$6,$7}')
}


gen_task_and_run_and_msd_range () {
    while read -r line gk_vis_fst_calc gk_vis_lst_calc
    do
        xargs -I{} echo "{} $gk_vis_fst_calc $gk_vis_lst_calc" <"${DIR_PROJECT_PATHS}/${line}.txt"
    done < <( tail -n +2 "${FILE_TARGET_PROJECTS}" |
        awk '{print $1,$2,$3,$8,$9}')
}


gen_task_and_run_and_rmsd_range () {
    while read -r line gk_vis_fst_calc gk_vis_lst_calc
    do
        xargs -I{} echo "{} $gk_vis_fst_calc $gk_vis_lst_calc" <"${DIR_PROJECT_PATHS}/${line}.txt"
    done < <( tail -n +2 "${FILE_TARGET_PROJECTS}" |
        awk '{print $1,$2,$3,$10,$11}')
}


split_file() {
    sp=$1 # 何行おきにの出力か
    awk "NR%${sp}==1"
}

init_dir_result () {
    local task=$1
    local dirname_result=$2
    local dir_result="$task/Analysis/$dirname_result"
    if [ -d "$dir_result" ]; then
        rm -rf "$dir_result"
    fi
    mkdir -p "$dir_result"
}