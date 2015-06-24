with (import <nixpkgs> {}).pkgs;
let pkg = haskell-ng.packages.ghc7101.callPackage
            ({ mkDerivation, aeson, base, bytestring, containers
             , digestive-functors, HUnit, lens, lens-aeson, mtl, safe
             , scientific, stdenv, tasty, tasty-hunit, text, vector
             }:
             mkDerivation {
               pname = "digestive-functors-aeson";
               version = "1.1.12.1";
               src = ./.;
               buildDepends = [
                 aeson base containers digestive-functors lens lens-aeson safe text
                 vector
               ];
               testDepends = [
                 aeson base bytestring digestive-functors HUnit mtl scientific tasty
                 tasty-hunit text
               ];
               homepage = "http://github.com/ocharles/digestive-functors-aeson";
               description = "Run digestive-functors forms against JSON";
               license = stdenv.lib.licenses.gpl3;
             }) {};
in
  pkg.env
