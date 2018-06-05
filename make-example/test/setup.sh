#!/bin/bash

sudo rm -r ./makefiletest

stack build make-example && cp ../../.stack-work/install/x86_64-linux/lts-10.2/8.2.2/bin/make-example . && ./make-example



