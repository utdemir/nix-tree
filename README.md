# nixdu

Interactively browse the dependency graph of your Nix derivations.

[![asciicast](https://asciinema.org/a/XVVOPQuU6ZQ0vGuO8ejr4JB11.svg)](https://asciinema.org/a/XVVOPQuU6ZQ0vGuO8ejr4JB11)

## Installation

```
nix-env -iA exe -f https://github.com/utdemir/nixdu/archive/master.tar.gz
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
