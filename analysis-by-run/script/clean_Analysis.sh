#!/bin/bash

readonly DIR_ROOT="$(cd "$(dirname $0)/"&&pwd)"
readonly FILE_TASK_ALL="${DIR_ROOT:?}/../task-generate/task_all.txt"

while read task
do
    rm -r "${task:?}"/*
done <"${FILE_TASK_ALL:?}"
