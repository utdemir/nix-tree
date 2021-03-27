{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";

    qualified-imports-plugin = {
      url = "github:utdemir/qualified-imports-plugin/main";
      flake = false;
    };
  };

  description = "Interactively browse the dependency graph of your Nix derivations.";

  outputs = { self, nixpkgs, flake-utils, qualified-imports-plugin }:
    let
      overlay = self: super: {
        haskellPackages = super.haskellPackages.override {
          overrides = hself: hsuper: {
            nix-tree =
              hself.callCabal2nix
                "nix-tree"
                (self.lib.cleanSourceWith {
                  filter = path: _type:
                    !(builtins.elem (builtins.baseNameOf path) [ "asciicast.sh" "flake.nix" ]);
                  src = ./.;
                })
                { };
            qualified-imports-plugin =
              hself.callCabal2nix
                "qualified-imports-plugin"
                qualified-imports-plugin
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
            haskell-language-server
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
