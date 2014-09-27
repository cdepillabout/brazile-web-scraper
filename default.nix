{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: {
  pname = "brazile-web-scraping";
  version = "1.0.0";
  src = ./.;
  buildDepends = with haskellPackages; [
    ghcMod_5_0_1_2
    httpClient
    mtl
    transformers
    text
  ];
  buildTools = with haskellPackages; [ cabalInstall ];
  enableSplitObjs = false;
})
