{ pkgs ? import <nixpkgs> {} }:
let 
  project = import ./default.nix {};
in
  pkgs.mkShell {
    shellHook = ''
      export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
    '';
    name = "events-bot";
    buildInputs = project.env.nativeBuildInputs ++ (with pkgs; [ 
      haskellPackages.hpack 
      cabal2nix 
      haskellPackages.ghcide 
      cabal-install 
      zlib 
      stack 
      haskellPackages.hlint
      haskellPackages.brittany
    ]);
  }
