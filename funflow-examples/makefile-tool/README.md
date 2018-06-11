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
   fail gently in doing so.


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
and simply run it":

```bash
$ ls
makefile sourcefile1.c sourcefile2.c ...
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

## Known Issues

 * For some reason, it requires sudo to write files 
   inside dockers. This may be particular to some docker setups.


## Notes for writing the blog post


### How `buildTarget` works

 1) Get source files and make a Map SourceFile String for name -> content.
 2) Get the dependent make rules.
 3) Join the flows that build these dependent targets
 4) Recursively build these targets (caching ensures no repeated work)
 5) Use a compilation flow with the built dependencies and given
 source files to compile the input target.

### How `compileFile` works

 1) Write the given source files to the content store
 2) Merge all the dependent targets and sources to a folder in
 the content store.
 3) Make a bash executable that goes to an "input directory", runs 
 an input command and copies a hopefully produced output file to 
 an "output directory".
 4) Make a docker flow that takes an input directory, an input bash file 
 and runs that script with the input directory stored in the path "/input/deps"
 in the docker container.


### Docker Containers In General

 As I understand it, all we do is run a bash script
 in a docker container. We've preloaded files from the cache into
 /input/ and when we're done, we look for a (already specified) file in
 /output/, put that in the cache and return a CS.Item for that
 file in the cache.




  
