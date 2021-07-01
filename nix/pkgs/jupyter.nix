{symlinkJoin,
jupyterWith,
projectHaskellPackages,
docker
}:

let 
# The following haskell packages will be made available in the tutorial notebook environment
tutorialHaskellDependencies = p: with p; [
    funflow 
    regex-posix
    text
    containers
    JuicyPixels
    ihaskell-juicypixels
    process
];

iHaskell = jupyterWith.kernels.iHaskellWith {
    extraIHaskellFlags = "--codemirror Haskell";
    name = "haskell";
    customIHaskell = symlinkJoin {
        name="ihaskell-hnix"; 
        paths=[
            projectHaskellPackages.ihaskell.components.exes.ihaskell
            projectHaskellPackages.ihaskell.components.library
        ];
    };
    packages = tutorialHaskellDependencies;
    haskellPackages = projectHaskellPackages;
};

in jupyterWith.jupyterlabWith {
    kernels = [ iHaskell ];
    extraPackages = p: with p; [docker];
}
