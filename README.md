# nix-tree

![Build Status](https://github.com/utdemir/nix-tree/workflows/nix-build/badge.svg)
[![Packaging status](https://repology.org/badge/vertical-allrepos/haskell:nix-tree.svg)](https://repology.org/project/haskell:nix-tree/versions)

Interactively browse dependency graphs of Nix derivations.

[![asciicast](https://asciinema.org/a/cnilbmPXW51g97hdNJZcM5F6h.svg)](https://asciinema.org/a/cnilbmPXW51g97hdNJZcM5F6h)

## Installation

`nix-tree` is on `nixpkgs` since `20.09`, so just use your preferred method for adding packages to your system, eg:

```
nix-env -i nix-tree
```

To run the current development version:

```
nix-shell -p '(import (builtins.fetchTarball "https://github.com/utdemir/nix-tree/archive/main.tar.gz") {}).nix-tree' --run nix-tree
```

Or, if you use a Nix version with flake support:

```
nix run github:utdemir/nix-tree
```

## Usage

```
$ nix-tree --help
Usage: nix-tree [--version] [--derivation] [INSTALLABLE]
  Interactively browse dependency graphs of Nix derivations.

Available options:
  --version                Show the nix-tree version.
  --derivation             Operate on the store derivation rather than its
                           outputs.
  INSTALLABLE              A store path or a flake reference. Paths default to
                           "~/.nix-profile" and "/var/run/current-system".
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
nix-instantiate --no-out-link | xargs -o nix-tree

# Dependencies from shell.nix
nix-build shell.nix -A inputDerivation | xargs -o nix-tree

# All outputs of a derivation in nixpkgs:
nix-build '<nixpkgs>' -A openssl.all --no-out-link | xargs -o nix-tree
```

## Contributing

All contributions, issues and feature requests are welcome.

To hack on it, simply run `nix-shell` and use `cabal` as usual. Please run `./format.sh` before sending a PR.

## Related tools

* [nix-du](https://github.com/symphorien/nix-du)
* [nix-query-tree-viewer](https://github.com/cdepillabout/nix-query-tree-viewer)
* [nix-visualize](https://github.com/craigmbooth/nix-visualize)
