#!/bin/bash

readonly DIR_ROOT="$(cd "$(dirname "${0}")"; pwd)"

rm -r "${DIR_ROOT:?}/build"/*
