# Funflow tutorial

This folder holds the tutorials for `funflow`. They are written as a series of Jupyter notebooks.

<!-- toc -->

- [About](#about)
- [Setup from scratch](#setup-from-scratch)
  * [Snags and solutions](#snags-and-solutions)
  * [Establish Docker](#establish-docker)
- [Development](#development)

<!-- tocstop -->

## About

The tutorials are meant to progressively teach new users how to use the library and tools.
Tutorials are published as a part of the `funflow` documentation. They can be run locally using
the Jupyter Lab environment and IHaskell kernel defined in [shell.nix](./shell.nix):
<a name="run-nbs"></a>
```console
$ nix-shell
$ jupyter lab
```


## Setup from scratch
Here are a few steps and tips to avoid snags as you get started. Note that step 3 onward applies generally, for subsequent use even after installation/setup. Essentially, this section clarifies what to do before [running a notebook](#run-nbs), and what those commands are doing.
1. **Install Nix**, per the [Nix download page](https://nixos.org/download.html).
2. `cd` to the `funflow-tutorial` directory, as there's a `shell.nix` file specifically for this directory.
3. Fire up a Nix shell: `nix-shell`
4. Spin up a Jupyter notebook: `jupyter lab`

**Note**: if you create a fresh notebook to tinker with Funflow, choose `Haskell - haskell` as the notebook kernel if given a choice between that and simply `Haskell`.

### Snags and solutions
<a name="getting-Nix"></a>
1. **Getting Nix**: The main instruction provides a command to download with `curl` and then install. If your setup runs aground here, there are [more options](https://nixos.org/download.html#nix-more) to download from the Nix site and then install, including from source. Before trying this option, though, it may be worth a updating/updrading packages. This worked for at least one Tweager, as the attempt to download and install was being made from a fresh Ubuntu 20 installation. Use `sudo` as desired/needed.
    ```console
    apt-get update
    apt-get upgrade
    apt-get install curl
    ```
2. **Running a notebook**: If you hit a snag starting up and/or running a notebook, check that you've started `nix-shell` from the proper folder, namely the one with this doc.
3. If, when executing a notebook (e.g., `CCompilation.ipynb`), you hit a network / HTTPRequest error, try [ensuring Docker's up and running](#establish-docker).

### Establish Docker
Some tutorials require that Docker is up and running on your machine. Check out the official Docker website for more about [installation](https://docs.docker.com/engine/install/), and perhaps about the [Docker daemon](https://docs.docker.com/config/daemon/).


## Development

The tutorials are written in IPython notebook files in the [notebooks/](./notebooks) directory. Any
IHaskell notebook stored there will be included with the `funflow` documentation. Tutorials
with additional supporting files should be placed in a subdirectory, e.g. `notebooks/WordCount/WordCount.ipynb`.
