#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR
. "$(dirname $0)/lib/common_1run.sh"


parallel_analysis 'temp.sh'
parallel_analysis 'gr.sh'
parallel_analysis 'molecular_orientation.sh'
parallel_analysis 'thcd.sh'
parallel_analysis 'vis.sh'
parallel_analysis 'msd.sh'
parallel_analysis 'rmsd.sh'
