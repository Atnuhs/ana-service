#!/bin/bash

readonly DIR_TASK="$(cd "$(dirname $0)/../task"&&pwd)"
readonly FILE_TASK_ALL="${DIR_TASK:?}/task_all.txt"

while read task
do
    echo 
    rm -r "${task:?}"/*
done <"${FILE_TASK_ALL:?}"
