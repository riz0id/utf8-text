{ ghc ? "ghc902" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    utf8-text
    fourmolu
    haskell-language-server
    hlint; 
    
  inherit (pkgs) 
    cabal-install 
    clang 
    llvm;

}

