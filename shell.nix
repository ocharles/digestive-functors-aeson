{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers
      , digestive-functors, HUnit, lens, lens-aeson, mtl, safe
      , scientific, stdenv, tasty, tasty-hunit, text, vector
      }:
      mkDerivation {
        pname = "digestive-functors-aeson";
        version = "1.1.21";
        src = ./.;
        libraryHaskellDepends = [
          aeson base containers digestive-functors lens lens-aeson safe text
          vector
        ];
        testHaskellDepends = [
          aeson base bytestring digestive-functors HUnit mtl scientific tasty
          tasty-hunit text
        ];
        homepage = "http://github.com/ocharles/digestive-functors-aeson";
        description = "Run digestive-functors forms against JSON";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
