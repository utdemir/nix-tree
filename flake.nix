{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
    flake-compat = {
      url = "github:edolstra/flake-compat/master";
      flake = false;
    };
  };

  description = "Interactively browse the dependency graph of your Nix derivations.";

  outputs = { self, nixpkgs, flake-utils, ... }:
    let
      overlay = se: su: {
        haskellPackages = su.haskellPackages.override {
          overrides = hse: _hsu: {
            "nix-tree" = hse.callCabal2nix "nix-tree" self { };

            # Update brick until https://github.com/NixOS/nixpkgs/pull/202022/ is merged
            "brick" = hse.brick_1_4;
            "bimap" = hse.bimap_0_5_0;
            "text-zipper" = hse.text-zipper_0_12;
            "vty" = hse.vty_5_37;
          };
        };
        nix-tree =
          se.haskell.lib.justStaticExecutables
            se.haskellPackages.nix-tree;
      };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      in
      {
        defaultPackage = pkgs.nix-tree;
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p."nix-tree" ];
          buildInputs = [
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.ormolu
            pkgs.haskellPackages.hlint
            pkgs.nixpkgs-fmt
            pkgs.bash
          ];
          withHoogle = false;
        };
      }
    );
}
