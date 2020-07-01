let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {
    overlays = [ (import ./overlay.nix) ];
  };

  shell = pkgs.haskellPackages.shellFor {
    packages = p: [
      p."nixdu"
    ];
    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      ormolu
      hlint
      steeloverseer
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };
in
{
  inherit shell;
  inherit (pkgs) nixdu;
}
