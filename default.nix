{ compiler ? "ghc8101" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "nixdu" =
        hself.callCabal2nix
          "nixdu"
          (gitignore ./.) { };
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."nixdu"
    ];
    buildInputs = with pkgs.haskellPackages; [
      myHaskellPackages.cabal-install
      ghcid
      ormolu
      hlint
      steeloverseer
      (import sources.niv { }).niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."nixdu");

  docker = pkgs.dockerTools.buildImage {
    name = "nixdu";
    config.Cmd = [ "${exe}/bin/nixdu" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "nixdu" = myHaskellPackages."nixdu";
}
