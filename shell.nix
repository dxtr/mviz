(import ./default.nix).shellFor {
  tools = {
    cabal = "latest";
    stylish-haskell = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
  };
}
