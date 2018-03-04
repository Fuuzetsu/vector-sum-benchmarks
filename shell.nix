{ nixpkgs ? import ./nixpkgs.nix {}
, ghc ? nixpkgs.haskell.compiler.ghc822
}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "vector-sum-benchmarks";
  buildInputs = [ ];
  inherit ghc;
  LANG = "en_US.utf8";
}
