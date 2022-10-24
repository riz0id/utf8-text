{ ghc }:

import (import ./nixpkgs.nix) {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions [  
      (import exts/text.nix {
        inherit ghc;
      })
      (import exts/utf8-text.nix {
        inherit ghc;
      })
    ] pkgs pkgs;
}