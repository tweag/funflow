#!/usr/bin/env bash
cd /input/deps
    g++ main.o factorial.o hello.o -o hello
cp hello /output/