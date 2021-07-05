#!/bin/bash

readonly DIR_ANALYSIS_SERVICE="$(cd "$(dirname $0)/../.." && pwd)"
readonly DIR_TASK="$(cd "$(dirname $0)/.." && pwd)/task"
readonly FILE_HOSTS="${DIR_ANALYSIS_SERVICE:?}/setting/excution_hosts.tsv"
readonly FILENAME_EXEC_SCRIPT='ana_gr.sh'

echo "##### 解析の実行 #####"


tail -n +2 "${FILE_HOSTS:?}" | xargs -I{} -P0 bash -c "
read host_name num_core < <(echo {})
filename_task=\"task_\${host_name:?}.txt\"
file_task=${DIR_TASK:?}/\"\${filename_task}\"
ssh \"\${host_name:?}\" << EOF
cat \"\${file_task:?}\" | \\
xargs -I@ -P\${num_core:?} bash @/script/${FILENAME_EXEC_SCRIPT:?}
EOF
"
