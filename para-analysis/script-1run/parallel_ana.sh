#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR
. "$(dirname $0)/lib/common_1run.sh"


# 準備
$(dirname $0)/../../analysis-1run/script/build.sh
$(dirname $0)/gen_task_all.sh
$(dirname $0)/gen_task_div.sh

# 解析
# parallel_analysis 'temp.sh'
# parallel_analysis 'gr.sh'
# parallel_analysis 'molecular_orientation.sh'
# parallel_analysis 'GK_thcd.sh'
parallel_analysis 'thcd.sh'
# parallel_analysis 'vis.sh'
# parallel_analysis 'msd.sh'
# parallel_analysis 'rmsd.sh'
