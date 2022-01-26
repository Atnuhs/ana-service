#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR
. "$(dirname $0)/lib/common_allrun.sh"

# 準備
# $(dirname $0)/../../analysis-allrun/script/clean.sh
$(dirname $0)/../../analysis-allrun/script/build.sh
$(dirname $0)/make_task_all.sh
$(dirname $0)/make_task_div.sh

# 解析の削除
# clean_analysis # [!注意!] taskのAnalysisを全部消す

# 解析
parallel_analysis 'temp.sh'
parallel_analysis 'gr.sh'
# parallel_analysis 'ext_gr.sh'
# parallel_analysis 'molecular_orientation.sh'
parallel_analysis 'GK_thcd.sh'
parallel_analysis 'thcd.sh'
parallel_analysis 'GK_vis.sh'
parallel_analysis 'vis.sh'
parallel_analysis 'msd.sh'
parallel_analysis 'rmsd.sh'
