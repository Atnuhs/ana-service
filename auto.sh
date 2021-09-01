#!/bin/bash
set -eu -o pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

./analysis-by-run/script/gen_task_all
./analysis-by-run/script/gen_task_div
./analysis-by-run/script/distribute_Analysis
./analysis-by-run/script/temp.sh
./analysis-by-run/script/msd.sh
./analysis-by-run/script/thcd.sh
./analysis-by-run/script/vis.sh

./analysis-by-project/script/gen_task_all
./analysis-by-project/script/gen_task_div
./analysis-by-project/script/distribute_Analysis
./analysis-by-project/script/para_temp
./analysis-by-project/script/para_msd
./analysis-by-project/script/para_thcd
./analysis-by-project/script/para_vis