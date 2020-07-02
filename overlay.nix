self: super: {
  haskellPackages =
    super.haskellPackages.override {
      overrides = hself: hsuper: {
        nix-tree =
          hself.callCabal2nix
            "nix-tree"
            (self.nix-gitignore.gitignoreSourcePure
              [ ./.gitignore "asciicast.sh" "*.nix" ]
              ./.
            ) { };
      };
    };

  nix-tree =
    self.haskell.lib.justStaticExecutables
      self.haskellPackages.nix-tree;

  nixdu =
    self.lib.warn "nixdu is renamed to nix-tree." self.nix-tree;
}
