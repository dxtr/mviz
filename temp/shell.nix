{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, containers
      , dear-imgui, explicit-exception, hashable, jack, lib, monad-logger
      , mtl, OpenGL, sdl2, StateVar, stm, template-haskell, text, time
      , transformers, unordered-containers
      }:
      mkDerivation {
        pname = "mviz";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          async base bytestring containers dear-imgui explicit-exception
          hashable jack monad-logger mtl OpenGL sdl2 StateVar stm
          template-haskell text time transformers unordered-containers
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [ base ];
        description = "Audio visualizer";
        license = lib.licenses.isc;
        mainProgram = "mviz";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskell.packages.ghc96
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
