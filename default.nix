let
  sources = import ./nix/sources.nix;
in

{ pkgs ? import sources.nixpkgs { } }:

let
  nix-filter = import sources.nix-filter;
  src = nix-filter {
    root = ./.;
    include = with nix-filter; [
      (and
        (or_ (inDirectory "src") (inDirectory "test"))
        (or_ isDirectory (matchExt "hs")))
      ./nix-tree.cabal
      ./README.md
      ./CHANGELOG.md
      ./LICENSE
    ];
  };

  myHaskellPackages = pkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      "nix-tree" =
        hself.callCabal2nix "nix-tree" src { };
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."nix-tree"
    ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.haskellPackages.hlint
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = false;
  };

in
{
  inherit shell;
  "nix-tree" =
    pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."nix-tree");
}

