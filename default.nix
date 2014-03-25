{}:
with import <nixpkgs> {};
let
  inherit (haskellPackages) cabal cabalInstall digestiveFunctors lens safe vector HUnit tasty tastyHunit;

in cabal.mkDerivation (self: {
  pname = "digestive-functors-aeson";
  version = "1.1.2";
  src = ./.;
  buildDepends = [ digestiveFunctors lens safe vector ];
  buildTools = [ cabalInstall ];
  testDepends = [ HUnit tasty tastyHunit ];
})
