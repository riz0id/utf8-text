{ ghc ? "ghc924" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.utf8-text