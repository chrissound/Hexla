{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, byteunits, containers, directory
      , filepath, hpack, optparse-applicative, rainbow, rainbox, split
      , stdenv, string-conversions, text, time, typed-process, unix
      }:
      mkDerivation {
        pname = "hexla";
        version = "0.2.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base byteunits containers directory filepath optparse-applicative
          rainbow rainbox split string-conversions text time typed-process
          unix
        ];
        preConfigure = "hpack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
