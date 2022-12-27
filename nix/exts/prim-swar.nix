{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        prim-swar = self.callPackage ../pkgs/prim-swar.nix { };
      });
    };
  };
}