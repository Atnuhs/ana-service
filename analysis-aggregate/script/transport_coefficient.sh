#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

# shellcheck source=/dev/null
. "$(dirname "$0")/lib/common.sh"

echo "### $0"

while read -r project_name
do
    dir_result="$(dir_result "$project_name")"
    mkdir -p "${dir_result}"
    echo "$dir_result"

    file_result="${dir_result}/transport_coefficient.tsv"
    file_md_param="${dir_result}/md_parameter.tsv"
    file_thcd="${dir_result}/thcd.tsv"
    file_viscousity="${dir_result}/viscousity.tsv"
    file_tdc="${dir_result}/translational_diffusion_coefficient.tsv"
    file_rdc="${dir_result}/rotational_diffusion_coefficient.tsv"

    join -t $'\t' "$file_thcd" "$file_viscousity" > "${file_result}.tmp1"
    join -t $'\t' "$file_tdc" "$file_rdc" > "${file_result}.tmp2"
    join -t $'\t' "${file_result}.tmp1" "${file_result}.tmp2" > "${file_result}.tmp3"
    join -t $'\t' "$file_md_param" "${file_result}.tmp3" > "$file_result"
    rm "${file_result}.tmp1" "${file_result}.tmp2" "${file_result}.tmp3"

done < "${FILE_TARGET_PROJECTS}"