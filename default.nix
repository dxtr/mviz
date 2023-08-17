let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};
  # If ./nix/sources.nix file is not found run:
  #   niv init
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix {};
  # If haskellNix is not found run:
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
  pkgPlan = haskellNix.callCabalProjectToNix {
    src = ./.;
  };
  pkgSet = haskellNix.mkCabalProjectPkgSet {
    plan-pkgs = import pkgPlan;
    pkg-def-extras = [];
    modules = [];
  };
in pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.cleanSourceHaskell {
    name = "mviz";
    src = ./.;
  };
  # 'cleanGit' cleans a source directory based on the files known by git
  # src = pkgs.haskell-nix.haskellLib.cleanGit {
  #   name = "haskell-nix-project";
  #   src = ./.;
  # };
  # Specify the GHC version to use.
  compiler-nix-name = "ghc96"; # Not required for `stack.yaml` based projects.
}
