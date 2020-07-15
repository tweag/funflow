#!/bin/sh
#!nix-shell -p stack

stack build funflow2-tutorial \
    && echo "Generating html..." \
    && stack --verbosity silent run funflow2-tutorial:exe:tutorial1 > out/tutorial1.html \
    && echo "done"