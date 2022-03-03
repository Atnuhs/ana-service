#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

cd "$(dirname "$0")"

# ./aggregate_axis_function.sh
# ./aggregate_md_condition.sh

# ./aggregate_thcd.sh
# ./aggregate_viscousity.sh
# ./aggregate_t_diff_coef.sh
# ./aggregate_r_diff_coef.sh

./aggregate_gr.sh
./aggregate_iemls.sh
./aggregate_ismls.sh
