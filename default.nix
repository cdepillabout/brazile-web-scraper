{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall
    text mtl transformers; # Haskell dependencies here

in cabal.mkDerivation (self: {
  pname = "brazile-web-scraping";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    # As imported above
    text mtl transformers
  ];
  buildTools = [ cabalInstall ];
  enableSplitObjs = false;
})
