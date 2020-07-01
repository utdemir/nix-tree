# nixdu

Interactively browse the dependency graph of your Nix derivations.

[![asciicast](https://asciinema.org/a/XVVOPQuU6ZQ0vGuO8ejr4JB11.svg)](https://asciinema.org/a/XVVOPQuU6ZQ0vGuO8ejr4JB11)

## Installation

```
nix-env -iA nixdu -f https://github.com/utdemir/nixdu/archive/master.tar.gz
```

A nixpkgs overlay is also provided via `overlay.nix`, that can be used
with tools like [home-manager][]:

```nix
nixpkgs.overlays = [
  (let url = https://github.com/utdemir/nixdu/archive/master.tar.gz;
    in import "${builtins.fetchTarball url}/overlay.nix" {})
];

home.packages = [ pkgs.nixdu ];
```

## Usage

```
$ nixdu --help
nixdu --help
Usage: nixdu [paths] [-h|--help]
  Paths default to $HOME/.nix-profile and /var/run/current-system.
Keybindings:
  hjkl/Arrow Keys : Navigate
  q/Esc:          : Quit / close modal
  w               : Open why-depends mode
  i               : Toggle modeline
  ?               : Show help
```

[home-manager]: https://github.com/rycee/home-manager
