{ pkgs ? import <nixpkgs> {config.allowBroken=true;} }:
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          discord-haskell = haskellPackagesNew.callPackage ./nix/discord-haskell.nix {};
          rrule = haskellPackagesNew.callPackage ./nix/rrule.nix {};

          events-bot =
            haskellPackagesNew.callPackage ./events-bot.nix { };
        };
      };
    };
  };

  mypkgs = import <nixpkgs> { inherit config; };
in
mypkgs.haskellPackages.events-bot
