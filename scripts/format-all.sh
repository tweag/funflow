#! /usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
find $(dirname $DIR) -name "*.hs" -not -path "*.stack-work/*" -printf '%p\n' -exec ormolu --mode inplace {} \;
