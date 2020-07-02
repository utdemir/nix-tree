# nix-tree

![Build Status](https://github.com/utdemir/nix-tree/workflows/nix-build/badge.svg)

Interactively browse the dependency graph of your Nix derivations.

[![asciicast](https://asciinema.org/a/ahDveBL1gs5t36z1myePtrKpR.svg)](https://asciinema.org/a/ahDveBL1gs5t36z1myePtrKpR)

## Installation

```
nix-env -iA nix-tree -f https://github.com/utdemir/nix-tree/archive/master.tar.gz
```

A nixpkgs overlay is also provided via `overlay.nix`, that can be used
with tools like [home-manager][]:

```nix
nixpkgs.overlays = [
  (let url = https://github.com/utdemir/nix-tree/archive/master.tar.gz;
    in import "${builtins.fetchTarball url}/overlay.nix" {})
];

home.packages = [ pkgs.nix-tree ];
```

## Usage

```
$ nix-tree --help
nix-tree --help
Usage: nix-tree [paths] [-h|--help]
  Paths default to $HOME/.nix-profile and /var/run/current-system.
Keybindings:
  hjkl/Arrow Keys : Navigate
  q/Esc:          : Quit / close modal
  w               : Open why-depends mode
  i               : Toggle modeline
  ?               : Show help
```

[home-manager]: https://github.com/rycee/home-manager

# Related tools

* [nix-du](https://github.com/symphorien/nix-du)
* [nix-query-tree-viewer](https://github.com/cdepillabout/nix-query-tree-viewer)
* [nix-visualize](https://github.com/craigmbooth/nix-visualize)
