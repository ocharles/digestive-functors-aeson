{}:
with import <nixpkgs> {};
let
  inherit (haskellPackages) aeson cabal cabalInstall digestiveFunctors lens
    lensAeson safe vector HUnit tasty tastyHunit;

in cabal.mkDerivation (self: {
  pname = "digestive-functors-aeson";
  version = "1.1.2";
  src = ./.;
  buildDepends = [ aeson digestiveFunctors lens lensAeson safe vector ];
  buildTools = [ cabalInstall ];
  testDepends = [ HUnit tasty tastyHunit ];
})
