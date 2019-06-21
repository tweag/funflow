{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "funflow-cwl"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "divesh.otwani@tweag.io";
      author = "Divesh Otwani";
      homepage = "https://github.com/githubuser/cwl-funflow#readme";
      url = "";
      synopsis = "A CWL implementation using Funflow.";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.async)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.directory)
          (hsPkgs.exceptions)
          (hsPkgs.filepath)
          (hsPkgs.funflow)
          (hsPkgs.hashable)
          (hsPkgs.ilist)
          (hsPkgs.katip)
          (hsPkgs.parsec)
          (hsPkgs.path)
          (hsPkgs.path-io)
          (hsPkgs.process)
          (hsPkgs.scientific)
          (hsPkgs.text)
          (hsPkgs.transformers)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          (hsPkgs.yaml)
          ];
        };
      exes = {
        "ffcwlrunner" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.directory)
            (hsPkgs.funflow)
            (hsPkgs.funflow-cwl)
            (hsPkgs.hedis)
            (hsPkgs.network)
            (hsPkgs.optparse-applicative)
            (hsPkgs.path)
            (hsPkgs.path-io)
            ];
          };
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.funflow)
            (hsPkgs.funflow-cwl)
            (hsPkgs.path)
            (hsPkgs.path-io)
            (hsPkgs.safe-exceptions)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.transformers)
            (hsPkgs.yaml)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././funflow-cwl; }