#!/bin/sh
#!nix-shell -p stack

echo "# Building tutorials ..."

# Get path to folder
# https://stackoverflow.com/questions/4774054/reliable-way-for-a-bash-script-to-get-the-full-path-to-itself
DIR="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

echo "## Cleaning output folder ..."

rm -rf $DIR/out 
mkdir -p $DIR/out

echo "## Generating html files ..."

stack --verbosity silent run quick-reference > $DIR/out/quick-reference.html
stack --verbosity silent run tutorial1 > $DIR/out/tutorial1.html

echo "## Done."
