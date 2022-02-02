#!/bin/bash
set -euo pipefail
trap 'echo "ERROR: line no = $LINENO, exit status = $?" >&2; exit 1' ERR

readonly TARGET='f2_moment_of_inertia'
readonly FST_RUN='2'
readonly LST_RUN='50'
readonly FST_CALC='4000'
readonly LST_CALC='6000'
readonly PARA_NUM='8'

./temp.sh "$TARGET" "$FST_RUN" "$LST_RUN" "$PARA_NUM"
./gr.sh "$TARGET" "$FST_RUN" "$LST_RUN" "$PARA_NUM"
./GK_thcd.sh "$TARGET" "$FST_RUN" "$LST_RUN" "$PARA_NUM"
./GK_viscousity.sh "$TARGET" "$FST_RUN" "$LST_RUN" "$PARA_NUM"
./thcd.sh "$TARGET" "$FST_CALC" "$LST_CALC"
./viscousity.sh "$TARGET" "$FST_CALC" "$LST_CALC"
./msd.sh "$TARGET" "$FST_RUN" "$LST_RUN" "$PARA_NUM"
./rmsd.sh "$TARGET" "$FST_RUN" "$LST_RUN" "$PARA_NUM"
