self: super: {
  haskellPackages =
    super.haskellPackages.override {
      overrides = hself: hsuper: {
        nixdu =
          hself.callCabal2nix
            "nixdu"
            (self.nix-gitignore.gitignoreSourcePure
              [ ./.gitignore "asciicast.sh" "*.nix" ]
              ./.
            ) { };
      };
    };

  nixdu =
    self.haskell.lib.justStaticExecutables
      self.haskellPackages.nixdu;
}
