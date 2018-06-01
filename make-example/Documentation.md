# Documentation

## Introduction

A common workflow is that of a build tool.
A `Makefile`-like structure defines a workflow for taking source files and turning 
them into target files.

This program takes a similarly structured `Makefile` and 
creates the target files specified (gracefully handling failures).


## Specification

For now, it just deals with compiling things with gcc.

The file looks like this:

```makefile

source-files: file1.c file2.h file2.c file3.h file4.h file5.c

exec : file1.o file3.o
        gcc file1.o file2.o -o exec

file2.o : file3.o ...
          ...

```

We attempt to compile it in a way such that

 * we catch bad or non-existant makefiles
 * on successive runs, we don't re-make targets we don't need to (caching)
 * if we fail, we indicate the target that can't be built.





