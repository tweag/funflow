{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "funflow-examples"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "andreas.herrmann@tweag.io";
      author = "Andreas Herrmann, Divesh Otwani";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      exes = {
        "external-c-computation" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.funflow)
            (hsPkgs.path)
            (hsPkgs.path-io)
            (hsPkgs.text)
            ];
          };
        "makefile-tool" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.funflow)
            (hsPkgs.path)
            (hsPkgs.path-io)
            (hsPkgs.text)
            (hsPkgs.katip)
            (hsPkgs.safe-exceptions)
            (hsPkgs.data-default)
            (hsPkgs.directory)
            (hsPkgs.parsec)
            (hsPkgs.unix)
            (hsPkgs.bytestring)
            ];
          };
        "omdb" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.async)
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.funflow)
            (hsPkgs.lens)
            (hsPkgs.lens-aeson)
            (hsPkgs.optparse-generic)
            (hsPkgs.path)
            (hsPkgs.path-io)
            (hsPkgs.safe-exceptions)
            (hsPkgs.text)
            (hsPkgs.wreq)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././funflow-examples; }