{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "funflow"; version = "1.4.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "nicholas.clarke@tweag.io";
      author = "Tom Nielsen, Nicholas Clarke, Andreas Herrmann";
      homepage = "https://github.com/tweag/funflow";
      url = "";
      synopsis = "Workflows with arrows";
      description = "An arrow with resumable computations and logging";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.Glob)
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.clock)
          (hsPkgs.constraints)
          (hsPkgs.containers)
          (hsPkgs.contravariant)
          (hsPkgs.cryptonite)
          (hsPkgs.data-default)
          (hsPkgs.directory)
          (hsPkgs.exceptions)
          (hsPkgs.filepath)
          (hsPkgs.ghc-prim)
          (hsPkgs.hashable)
          (hsPkgs.hedis)
          (hsPkgs.hostname)
          (hsPkgs.integer-gmp)
          (hsPkgs.katip)
          (hsPkgs.lens)
          (hsPkgs.lifted-async)
          (hsPkgs.memory)
          (hsPkgs.monad-control)
          (hsPkgs.mtl)
          (hsPkgs.path)
          (hsPkgs.path-io)
          (hsPkgs.pretty)
          (hsPkgs.process)
          (hsPkgs.random)
          (hsPkgs.safe-exceptions)
          (hsPkgs.scientific)
          (hsPkgs.sqlite-simple)
          (hsPkgs.stm)
          (hsPkgs.store)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.unix)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          (hsPkgs.yaml)
          ] ++ (if system.isLinux
          then [ (hsPkgs.hinotify) ]
          else (pkgs.lib).optional (system.isOsx || system.isFreebsd) (hsPkgs.kqueue));
        };
      exes = {
        "ffexecutord" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.clock)
            (hsPkgs.funflow)
            (hsPkgs.hedis)
            (hsPkgs.network)
            (hsPkgs.path)
            (hsPkgs.text)
            (hsPkgs.unix)
            (hsPkgs.safe-exceptions)
            (hsPkgs.optparse-applicative)
            ];
          };
        };
      tests = {
        "test-funflow" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.funflow)
            (hsPkgs.filepath)
            (hsPkgs.hedis)
            (hsPkgs.path)
            (hsPkgs.path-io)
            (hsPkgs.text)
            (hsPkgs.safe-exceptions)
            (hsPkgs.unix)
            ];
          };
        "unit-tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.async)
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.funflow)
            (hsPkgs.path)
            (hsPkgs.path-io)
            (hsPkgs.process)
            (hsPkgs.random)
            (hsPkgs.safe-exceptions)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.temporary)
            (hsPkgs.unix)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././funflow; }