{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, containers
      , digestive-functors, HUnit, lens, lens-aeson, mtl, safe
      , scientific, stdenv, tasty, tasty-hunit, text, vector
      }:
      mkDerivation {
        pname = "digestive-functors-aeson";
        version = "1.1.18";
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

  overrides = self: super: {
    lens-aeson = pkgs.haskell.lib.doJailbreak super.lens-aeson;
    aeson =
      self.callPackage
        ({ mkDerivation, attoparsec, base, bytestring, containers, deepseq
         , dlist, fail, ghc-prim, hashable, HUnit, mtl, QuickCheck
         , quickcheck-instances, scientific, semigroups, stdenv, syb
         , template-haskell, test-framework, test-framework-hunit
         , test-framework-quickcheck2, text, time, transformers
         , unordered-containers, vector
         }:
         mkDerivation {
           pname = "aeson";
           version = "0.11.0.0";
           sha256 = "1mdd4klbad1dx5854agiiixfcc269hnmbam31zmfs91qszaljf5d";
           libraryHaskellDepends = [
             attoparsec base bytestring containers deepseq dlist fail ghc-prim
             hashable mtl scientific semigroups syb template-haskell text time
             transformers unordered-containers vector
           ];
           testHaskellDepends = [
             attoparsec base bytestring containers ghc-prim HUnit QuickCheck
             quickcheck-instances template-haskell test-framework
             test-framework-hunit test-framework-quickcheck2 text time
             unordered-containers vector
           ];
           homepage = "https://github.com/bos/aeson";
           description = "Fast JSON parsing and encoding";
           license = stdenv.lib.licenses.bsd3;
         }) {};
  };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages.override { inherit overrides; }
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {
          aeson = haskellPackages.callPackage (
                  { mkDerivation, attoparsec, base, bytestring, containers, deepseq
                  , dlist, fail, ghc-prim, hashable, HUnit, mtl, QuickCheck
                  , quickcheck-instances, scientific, semigroups, stdenv, syb
                  , template-haskell, test-framework, test-framework-hunit
                  , test-framework-quickcheck2, text, time, transformers
                  , unordered-containers, vector
                  }:
                  mkDerivation {
                    pname = "aeson";
                    version = "0.11.0.0";
                    sha256 = "1mdd4klbad1dx5854agiiixfcc269hnmbam31zmfs91qszaljf5d";
                    libraryHaskellDepends = [
                      attoparsec base bytestring containers deepseq dlist fail ghc-prim
                      hashable mtl scientific semigroups syb template-haskell text time
                      transformers unordered-containers vector
                    ];
                    testHaskellDepends = [
                      attoparsec base bytestring containers ghc-prim HUnit QuickCheck
                      quickcheck-instances template-haskell test-framework
                      test-framework-hunit test-framework-quickcheck2 text time
                      unordered-containers vector
                    ];
                    homepage = "https://github.com/bos/aeson";
                    description = "Fast JSON parsing and encoding";
                    license = stdenv.lib.licenses.bsd3;
                  }) {}; 
        };

in

  if pkgs.lib.inNixShell then drv.env else drv
