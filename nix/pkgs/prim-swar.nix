{ mkDerivation, base, fetchgit, ghc-prim, lib, template-haskell }:
mkDerivation {
  pname = "prim-swar";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/swar";
    sha256 = "0gwm8bxs9qn7vpclh2a1sdh9myjglpnyiyv571p1v4rmg0dnc835";
    rev = "de99d5900a83038bea074f86d6b5eeabb901f296";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/prim-swar; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base ghc-prim template-haskell ];
  homepage = "https://github.com/riz0id/swar";
  description = "TODO";
  license = lib.licenses.isc;
}
