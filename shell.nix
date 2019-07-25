{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false , withHoogle ? true}:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, byteunits, containers, directory
      , filepath, hpack, rainbox, stdenv, time, unix
      }:
      mkDerivation {
        pname = "app";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base byteunits containers directory filepath rainbox time unix
        ];
        preConfigure = "hpack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages' = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages = (
    if withHoogle
    then  haskellPackages'.override {
      overrides = (self: super:
        {
          ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = self.ghc.withPackages;
        }
      );
    }
    else haskellPackages'
  );

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
