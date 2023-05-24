# Name

`nix-tree` - Interactively browse dependency graphs of Nix derivations.

# Synopsis

`nix-tree` 
  `[INSTALLABLE]`
  `[--version]`
  `[--derivation]`
  `[--impure]`
  `[--help]`

# Description

## Keybindings

*  hjkl/Arrow Keys : Navigate
*  w               : Open why-depends mode
*  /               : Open search mode
*  s               : Change sort order
*  y               : Yank selected path to clipboard
*  ?               : Show help
*  q/Esc           : Quit / close modal

## Glossary

* **NAR Size**: Size of the store path itself.
* **Closure size**: Total size of the store path and all its transitive dependencies.
* **Added size**:  Size of the store path, and all its _unique_ transitive
  dependencies. In other words, the cost of having that store path on top
  of all other paths.


### Examples

`nix-build` prints built paths to stdout, which can be piped conveniently
with `| xargs -o nix-tree`. Examples:

```bash
# Output of a local derivation
nix-build . --no-out-link | xargs -o nix-tree

# Build time dependencies (passing a `.drv` path)
nix-instantiate -r | xargs -o nix-tree --derivation

# Dependencies from shell.nix
nix-build shell.nix -A inputDerivation | xargs -o nix-tree

# All outputs of a derivation in nixpkgs:
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