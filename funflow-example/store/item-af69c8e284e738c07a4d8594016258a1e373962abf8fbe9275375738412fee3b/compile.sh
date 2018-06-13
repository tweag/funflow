#!/usr/bin/env bash
out="$1"
shift
gcc -o "$out" $@
