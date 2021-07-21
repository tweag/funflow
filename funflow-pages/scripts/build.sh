#!/usr/bin/env bash
# exit when any command fails
set -e

# Set paths
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
out="$SCRIPT_DIR"/../result/funflow

srcPagesIndex="$SCRIPT_DIR"/../src
srcFunflowTutorials="$SCRIPT_DIR"/../../funflow-tutorial

mkdir -p "$out"

# Docs index page
cp -r "$srcPagesIndex"/* "$out"/

# Copy API
mkdir -p "$out"/api
cp -r "$(nix-build -A api-docs)"/share/doc/* "$out"/api/
chmod -R +rwx "$out"/api/

# Make tutorials
mkdir -p /tmp/funflow/store
"$(nix-build -A generate-funflow-tutorials)"/bin/generate-funflow-tutorial "$srcFunflowTutorials"/notebooks "$out"/tutorials
