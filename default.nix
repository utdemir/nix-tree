{ system ? builtins.currentSystem }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { inherit system; };

  gitignore = extra:
    pkgs.nix-gitignore.gitignoreSourcePure ([ ./.gitignore ] ++ extra);

  myHaskellPackages = pkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      "nixdu" =
        hself.callCabal2nix
          "nixdu"
          (gitignore [ "asciicast.sh" ] ./.) { };
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
