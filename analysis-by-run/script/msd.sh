#!/bin/bash

readonly DIR_SCRIPT_ROOT="$(cd "$(dirname $0)" && pwd)"
readonly FILE_HOSTS="${DIR_SCRIPT_ROOT:?}/../setting/excution_hosts.tsv"
readonly FILENAME_EXEC_SCRIPT='ana_msd.sh'

echo "##### 解析の実行 #####"


tail -n +2 "${FILE_HOSTS:?}" | xargs -I{} -P0 bash -c "
read host_name num_core < <(echo {})
filename_task=\"task_\${host_name:?}.txt\"
file_task=${DIR_SCRIPT_ROOT:?}/../task-generate/\"\${filename_task}\"
ssh \"\${host_name:?}\" << EOF
cat \"\${file_task:?}\" | \\
xargs -I@ -P\${num_core:?} bash @/script/${FILENAME_EXEC_SCRIPT:?}
EOF
"
