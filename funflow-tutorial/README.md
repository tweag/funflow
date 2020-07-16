# Funflow tutorial

This folder holds the tutorials for `funflow`.

## About

The tutorials are meant to progressively teach new users how to use the library and tools.

## Development

### General

The tutorials are written in Haskell files `.hs`.
They use the library `inliterate`.

By inserting

```haskell
{-# OPTIONS_GHC -F -pgmF inlitpp #-}
```

at the top of the file, one can write the file like a Markdown file and at the same time include snippets of Haskell code.
Those snippets can be run or not.
See [https://github.com/diffusionkinetics/open/tree/master/inliterate](https://github.com/diffusionkinetics/open/tree/master/inliterate).

In order to display the result of an evaluation, the result type needs to be an instance of `AskInliterate` from `Inliterate.Import`.
Please write those instances in the common `lib/Lib.hs` file.
Those instances can then be imported writing

````haskell
```haskell top hide
import Lib ()
```
````

### Build

Each file can be rendered using

```bash
stack run funflow2-tutorial:exe:tutorial1
```

this will output the result to stdout.
In order to write it to file it is adviced to use

```bash
stack --verbosity silent run funflow2-tutorial:exe:tutorial1 > out/tutorial1.html
```

A bash script `update_tutorial.sh` can be executed to 

### Adding a new tutorial

In order to add a new tutorial:

- create a new Hasekell file
- add the inliterate language pragma
- import the Lib module
- add the build expression to the bash script