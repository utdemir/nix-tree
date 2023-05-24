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
nix run github:utdemir/nix-tree -- --help
```

## Usage

See [MANUAL.md](./MANUAL.md)

## Contributing

All contributions, issues and feature requests are welcome.

To hack on it, simply run `nix-shell` and use `cabal` as usual. Please run `./format.sh` before sending a PR.

## Related tools

* [nix-du](https://github.com/symphorien/nix-du): Visualise which gc-roots to delete to free some space in your nix store
* [nix-melt](https://github.com/nix-community/nix-melt): A ranger-like flake.lock viewer
* [nix-query-tree-viewer](https://github.com/cdepillabout/nix-query-tree-viewer): GTK viewer for the output of `nix-store --query --tree`
* [nix-visualize](https://github.com/craigmbooth/nix-visualize): Uses the Nix package manager to visualize the dependencies of a given package