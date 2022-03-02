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

# Build and copy Haddock
mkdir -p "$out"/api
stack haddock
cp -r "$(stack path --local-doc-root)" "$out"/api/
chmod -R +rwx "$out"/api/
# Add extra symlink for header "Contents" links
# mkdir -p "$out"/api/share/doc/
mv "${out}"/api/doc/* "${out}"/api
# ln -s -f "$out"/api/doc/index.html "$out"/api/index.html

# Make tutorials
mkdir -p /tmp/funflow/store
"$(nix-build nix/pkgs.nix -A generate-funflow-tutorials)"/bin/generate-funflow-tutorial "$srcFunflowTutorials"/notebooks "$out"/tutorials
