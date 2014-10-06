{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: {
  pname = "brazile-web-scraping";
  version = "1.0.1";
  #src = ./.;
  src = builtins.filterSource (path: type: baseNameOf path != "output") ./.;
  buildDepends = with haskellPackages; [
    ghcMod #_5_0_1_2
    httpClient
    mtl
    transformers
    text
  ];
  buildTools = with haskellPackages; [ cabalInstall ];
  enableSplitObjs = false;
})
