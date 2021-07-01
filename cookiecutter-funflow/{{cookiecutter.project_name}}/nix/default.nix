{ sources ? import ./sources.nix }:  
with   
  { overlay = _: pkgs:
      { niv = import sources.niv {};   
      };
  };
import sources.nixpkgs                 
  { overlays = [ overlay ] ; config = {}; }
