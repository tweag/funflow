# TODO Materialization

let pkgs = import ./nix/pkgs.nix;
in (import ./default.nix).shellFor {
  exactDeps = true;
  buildInputs = [ pkgs.stack pkgs.ormolu pkgs.niv pkgs.git pkgs.docker pkgs.zlib ];
  tools = {
    # Current default is 1.2.0.0 but we live on the edge
    haskell-language-server = "1.4.0.0";
  };
}
