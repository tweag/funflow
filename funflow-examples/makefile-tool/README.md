# Documentation of The `makefile-tool`

## Introduction

Building a C project is a well known workflow. We have several steps of 
compiling object files, some of which depend on each other and then, finally 
compiling the executable we want.

Here, we provide a simple tool similar to 
[GNU's Make](https://www.gnu.org/software/make/).

Our executable, the `makefile-tool`, takes a `Makefile`-like file
and 

1. creates a workflow for that particular build, and,
2. runs that workflow to either produce the desired executable or 
   fails gently in doing so.


## Specification

### Building

```bash
$ stack build makefile-tool
```

### Usage

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
   * Each rule
     - States the name of the file produced, a colon, and then a list
      of dependencies. The depenencies are either source files or 
      other files that need to be produced.
     - The following line lists a `gcc` command with the restriction that 
       *the output file is not named, i.e., there is no `-o` argument*

To build the C project, put all the source files 
and the `Makefile` in the  same directory and 
simply run the makefile-tool:

```bash
$ ls
Makefile sourcefile1.c sourcefile2.c ...
$ stack exec makefile-tool
```

### Guarantees

We build in a way such that

 * we catch bad or non-existant makefiles
 * on successive runs, we don't re-make 
   targets we don't need to (caching)
 * we fail gracefully and indicate the target 
   that can't be built.

### Example

```bash
$ cd .../funflow/funflow-exmaples/makefile-tool/test
$ stack exec makefile-tool
$ ls
factorial.cpp  functions.h  hello  hello.cpp  main.cpp  Makefile  makefiletest
$ ./hello
Hello World!
The factorial of 5 is 120
```

  
