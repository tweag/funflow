# Documentation of The `makefile-tool`

<!-- toc -->

- [Introduction](#introduction)
- [Specification](#specification)
  * [Using the included example](#using-the-included-example)
  * [Writing the `Makefile`](#writing-the-makefile)
  * [Guarantees](#guarantees)
- [What to look for](#what-to-look-for)

<!-- tocstop -->

## Introduction

Building a C project is a well known workflow: source files are compiled to 
object files, and eventually object files are linked to produce a specific 
target.

Here, we provide a simple tool similar to 
[GNU's Make](https://www.gnu.org/software/make/).
Our executable, the `makefile-tool`, takes a `Makefile`-like file
and...
1. creates a workflow for that particular build, and,
2. runs that workflow to either produce the desired executable or 
   fail gently.


## Specification

### Using the included example
From repo root:
```bash
$ nix-build -A makefile-tool
$ cd funflow-examples/makefile-tool/test
$ ../../../result/bin/makefile-tool
$ ./hello
```

### Writing the `Makefile`

The MakeFile should have filename `Makefile` 
and look like this:

```bash

source-files: file1.c file2.h file2.c file3.h file4.h file5.c

executable-name: file1.o file3.o
        gcc file1.o file2.o

file2.o: file3.o ...
        gcc <command> ...

file1.0: file-dependency
        gcc <command> ...

```

More precisely, the `Makefile` should follow these rules:

 * The first line lists out all the source files
 * After a newline, there's a newline sepearted list of rules
   * The first rule is the *default goal* to be made, i.e., the 
     executable we want at the end.
   * Rule format:
     - The first line states the name of the file produced, a colon, 
       and then a list of dependencies. The depenencies are either 
       source files or other files that need to be produced.
     - The following line lists a `gcc` command with the restriction that 
       **the output file is not named, i.e., there is no `-o` argument**.

To build the C project, put all the source files 
and the `Makefile` in the same directory and 
simply run the `makefile-tool`. See the [example](#example)

### Guarantees

We build in a way such that...

 * we catch bad or non-existant makefiles
 * on successive runs, we don't re-make 
   targets we don't need to (caching)
 * we fail gracefully and indicate the target 
   that can't be built.


## What to look for
This example is chosen not only for likely familiarity and utility to many, but 
also to demonstrate a few features of `funflow`. In particular, you may keep an eye 
out for:

 * use of `Arrows` as related syntax, closely tied with the `Flow` type
 * use of each of the three "smart constructors" for building a `Flow`
 * recursive calls to the target building function: larger flows may be built up recursively
 * "joining" a collection of "atomic" flows into a single flow that accepts a collection and outputs a collection (`flowJoin`)
 * single execution of the "joined" flow (in particular, a single pull of the relevant Docker image)

