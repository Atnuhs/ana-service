#!/bin/bash


./analysis-aggregate/script/aggregate_axis_function.sh
./analysis-aggregate/script/aggregate_md_condition.sh
./analysis-aggregate/script/aggregate_viscousity.sh
./analysis-aggregate/script/aggregate_thcd.sh
./analysis-aggregate/script/aggregate_t_diff_coef.sh
./analysis-aggregate/script/aggregate_r_diff_coef.sh

# ./analysis-aggregate/script/aggregate_gr.sh
# ./analysis-aggregate/script/aggregate_iemls.sh
# ./analysis-aggregate/script/aggregate_ismls.sh
./analysis-aggregate/script/aggregate_iemls.sh
