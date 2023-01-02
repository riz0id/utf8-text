args:

import (import ./nixpkgs.nix) {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions (map (f: f args) [  
      (import exts/tasty-hedgehog.nix)
      (import exts/utf8-text.nix)
    ]) pkgs pkgs;
}