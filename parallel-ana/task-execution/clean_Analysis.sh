#!/bin/bash

readonly DIR_ROOT="$(cd "$(dirname $0)/"&&pwd)"
readonly FILE_LIST_ANALYSIS="${DIR_ROOT:?}/../task-generate/list_analysis.txt"

while read task
do
    rm -r "${task:?}"/*
done <"${FILE_LIST_ANALYSIS:?}"
