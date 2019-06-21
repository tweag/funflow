{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "funflow-jobs"; version = "0.2.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "nick@topos.org.uk";
      author = "Tom Nielsen";
      homepage = "https://github.com/tweag/funflow";
      url = "";
      synopsis = "Support for distribution of flows in funflow.";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.exceptions)
          (hsPkgs.funflow)
          (hsPkgs.hedis)
          (hsPkgs.lens)
          (hsPkgs.monad-control)
          (hsPkgs.mtl)
          (hsPkgs.store)
          (hsPkgs.text)
          (hsPkgs.transformers-base)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././funflow-jobs; }