# nix-tree

![Build Status](https://github.com/utdemir/nix-tree/workflows/nix-build/badge.svg)
[![Packaging status](https://repology.org/badge/vertical-allrepos/haskell:nix-tree.svg?exclude_unsupported=1)](https://repology.org/project/haskell:nix-tree/versions)

Interactively browse dependency graphs of Nix derivations.

[![asciicast](https://asciinema.org/a/cnilbmPXW51g97hdNJZcM5F6h.svg)](https://asciinema.org/a/cnilbmPXW51g97hdNJZcM5F6h)

## Installation

`nix-tree` is on `nixpkgs` since `20.09`, so just use your preferred method for adding packages to your system, eg:

```
nix-env -iA nix-tree
```

Or, for flake enabled systems:

```
nix profile install 'nixpkgs#nix-tree'
```

To run the current development version:

```
nix run github:utdemir/nix-tree -- --help
```

## Usage

```console
$ nix-tree --help
Usage: nix-tree [INSTALLABLE] [--store STORE] [--version] [--derivation] [--impure] [--dot]

  Interactively browse dependency graphs of Nix derivations.

Available options:
  INSTALLABLE              A store path or a flake reference.
                           Paths default to "~/.nix-profile" and "/var/run/current-system"
  --store STORE            The URL of the Nix store, e.g. "daemon" or "https://cache.nixos.org"
                           See "nix help-stores" for supported store types and settings.
  --version                Show the nix-tree version
  --derivation             Operate on the store derivation rather than its outputs
  --impure                 Allow access to mutable paths and repositories
  --dot                    Print the dependency graph in dot format
  -h,--help                Show this help text

Keybindings:
  hjkl/Arrow Keys : Navigate
  w               : Open why-depends mode
  /               : Open search mode
  s               : Change sort order
  y               : Yank selected path to clipboard
  ?               : Show help
  q/Esc           : Quit / close modal
```

### Glossary

* **NAR Size**: Size of the store path itself.
* **Closure size**: Total size of the store path and all its transitive dependencies.
* **Added size**:  Size of the store path, and all its _unique_ transitive
  dependencies. In other words, the cost of having that store path on top
  of all other paths. See [issue #14] for a better explanation.

[issue #14]: https://github.com/utdemir/nix-tree/issues/14

### Tips

`nix-build` prints built paths to stdout, which can be piped conveniently
with `| xargs -o nix-tree`. Examples:

```bash
# Output of a local derivation
nix-build . --no-out-link | xargs -o nix-tree

# Build time dependencies (passing a `.drv` path)
nix-instantiate . | xargs -o nix-tree --derivation

# Dependencies from shell.nix
nix-build shell.nix -A inputDerivation | xargs -o nix-tree

# All outputs of a derivation in nixpkgs
nix-build '<nixpkgs>' -A openssl.all --no-out-link | xargs -o nix-tree
```

`nix-tree` also supports flake references:

```bash
# Build time dependencies of a flake on the current directory
nix-tree --derivation '.#'

# Same thing works for any flake reference
nix-tree --derivation 'nixpkgs#asciiquarium'
```

Run `nix-tree` on your current nixos system:

```bash
nix-tree /nix/var/nix/profiles/system
```

Run `nix-tree` on a flake reference of a nixosConfiguration:

```bash
nix build --print-out-paths --no-link '.#nixosConfigurations.foo.config.system.build.toplevel'
nix-tree '.#nixosConfigurations.foo.config.system.build.toplevel'
```

Query the binary cache before download, with the `--store` option:

```bash
# Query the runtime dependency of `stellarium` (2 GiB closure) without download
nix eval --raw 'nixpkgs#stellarium.outPath' | xargs -o nix-tree --store https://cache.nixos.org
```

For valid `--store` options, see [`nix help-stores`](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-help-stores). For example,

```bash
# Build in a temporary chroot store and examine the output
nix build --store /tmp/chroot-store 'nixpkgs#hello' --print-out-paths | xargs -o nix-tree --store /tmp/chroot-store
```

## Contributing

All contributions, issues and feature requests are welcome.

To hack on it, simply run `nix-shell` and use `cabal` as usual. Please run `./format.sh` before sending a PR.

## Related tools

* [nix-du](https://github.com/symphorien/nix-du): Visualise which gc-roots to delete to free some space in your nix store
* [nix-melt](https://github.com/nix-community/nix-melt): A ranger-like flake.lock viewer
* [nix-query-tree-viewer](https://github.com/cdepillabout/nix-query-tree-viewer): GTK viewer for the output of `nix-store --query --tree`
* [nix-visualize](https://github.com/craigmbooth/nix-visualize): Uses the Nix package manager to visualize the dependencies of a given package
