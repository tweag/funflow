let
  spec = builtins.fromJSON (builtins.readFile ./haskell.nix-src.json);
  src = {
    url = "${spec.url}/archive/${spec.rev}.tar.gz";
    inherit (spec) sha256;
  };

in 
  import (builtins.fetchTarball src)
