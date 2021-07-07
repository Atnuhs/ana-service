#!/bin/bash

readonly DIR_ANALYSIS_SERVICE="$(cd "$(dirname $0)/../.." && pwd)"
readonly DIR_TASK="$(cd "$(dirname $0)/.." && pwd)/task"
readonly FILE_HOSTS="${DIR_ANALYSIS_SERVICE:?}/setting/excution_hosts.tsv"
readonly FILENAME_EXEC_SCRIPT='ana_thcd.sh'

echo "##### 解析の実行 #####"


tail -n +2 "${FILE_HOSTS:?}" | xargs -I{} -P0 bash -c "
read host_name num_core <<<'{}'
filename_task=\"task_\${host_name:?}.tsv\"
file_task=${DIR_TASK:?}/\"\${filename_task:?}\"

tasks=\$( \\
    cat \"\${file_task:?}\" | \\
    awk -F \"\\t\" '{print \$1}' \\
    )

ssh \"\${host_name:?}\" << EOF
echo \"\${tasks:?}\" | \\
xargs -I@ -P\${num_core:?} bash @/script/${FILENAME_EXEC_SCRIPT:?}
EOF
"
