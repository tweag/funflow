{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "funflow-aws"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "nick@topos.org.uk";
      author = "Nicholas Clarke";
      homepage = "https://github.com/tweag/funflow";
      url = "";
      synopsis = "Tools for flows which interact with AWS";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.funflow)
          (hsPkgs.aeson)
          (hsPkgs.aws)
          (hsPkgs.conduit)
          (hsPkgs.conduit-extra)
          (hsPkgs.constraints)
          (hsPkgs.http-conduit)
          (hsPkgs.lens)
          (hsPkgs.path)
          (hsPkgs.path-io)
          (hsPkgs.reflection)
          (hsPkgs.resourcet)
          (hsPkgs.text)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././funflow-aws; }