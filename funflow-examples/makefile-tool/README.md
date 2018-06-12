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
$ pwd
.../funflow/funflow-examples/makefile-tool

$ sudo stack build makefile-tool --allow-different-user
makefile-tool-0.1.0.0: build (exe)                                  
Preprocessing executable 'makefile-tool' for makefile-tool-0.1.0.0..                        
Building executable 'makefile-tool' for makefile-tool-0.1.0.0..                                
[1 of 1] Compiling Main             
Linking.
makefile-tool-0.1.0.0: copy/register                                                           
Installing executable makefile-tool in 
.../funflow/.stack-work/install/x86_64-linux/lts-10.2/8.2.2/bin 
```

### Usage

The MakeFile should have filename `makefile` 
and look like this:

```bash

source-files: file1.c file2.h file2.c file3.h file4.h file5.c

executable-name: file1.o file3.o
        gcc file1.o file2.o -o exec

file2.o: file3.o ...
        gcc <command> ...

file1.0: file-dependency
        gcc <command> ...

```
Put all the source files and the makefile in the 
same directory as the `makefile-tool` executable 
and simply run it:

```bash
$ ls
makefile-tool Makefile sourcefile1.c sourcefile2.c ...
$ sudo ./makefile-tool
```

### Guarantees

We build in a way such that

 * we catch bad or non-existant makefiles
 * on successive runs, we don't re-make 
   targets we don't need to (caching)
 * we fail gracefully and indicate the target 
   that can't be built

### Example

```bash
$ pwd
funflow/funflow-exmaples/makefile-tool/test
$ sudo ./makefile-tool
```

  
