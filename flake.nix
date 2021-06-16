{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
  };

  description = "Interactively browse the dependency graph of your Nix derivations.";

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = self: super: {
        nix-tree =
          (self.callPackage ./default.nix { }).nix-tree;
      };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      in
      {
        defaultPackage = pkgs.nix-tree;
        devShell = (pkgs.callPackage ./default.nix { }).shell;
      }
    );
}
