#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

echo "### $0"

"${DIR_ROOT}/script/md_parameter.sh"
"${DIR_ROOT}/script/gr.sh"
"${DIR_ROOT}/script/gr_gg.sh"
"${DIR_ROOT}/script/gr_gp.sh"
"${DIR_ROOT}/script/gr_pp.sh"
"${DIR_ROOT}/script/gr_3d.sh"
"${DIR_ROOT}/script/molecular_orientation_gp.sh"
"${DIR_ROOT}/script/molecular_orientation_pp.sh"
"${DIR_ROOT}/script/thcd.sh"
"${DIR_ROOT}/script/viscousity.sh"
"${DIR_ROOT}/script/rotational_diffusion_coefficient.sh"
"${DIR_ROOT}/script/translational_diffusion_coefficient.sh"
"${DIR_ROOT}/script/integ_thcd.sh"
"${DIR_ROOT}/script/integ_viscousity.sh"
"${DIR_ROOT}/script/transport_coefficient.sh"
