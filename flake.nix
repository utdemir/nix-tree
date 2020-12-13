{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
  };

  description = "Interactively browse the dependency graph of your Nix derivations.";

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = self: super: {
        haskellPackages = super.haskellPackages.override {
          overrides = hself: hsuper: {
            nix-tree =
              hself.callCabal2nix
                "nix-tree"
                (self.nix-gitignore.gitignoreSourcePure
                  [ ./.gitignore "asciicast.sh" "flake.nix" ]
                  ./.
                )
                { };
          };
        };
        nix-tree =
          self.haskell.lib.justStaticExecutables
            self.haskellPackages.nix-tree;
      };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      in
      {
        defaultPackage = pkgs.nix-tree;
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [
            p."nix-tree"
          ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            ghcid
            ormolu
            hlint
            pkgs.nixpkgs-fmt
          ];
          withHoogle = false;
        };
      }
    );
}
