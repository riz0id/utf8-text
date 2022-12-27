{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        swar = self.callPackage ../pkgs/swar.nix { };
      });
    };
  };
}