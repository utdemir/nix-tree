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

## Glossary

* **NAR Size**: Size of the store path itself.
* **Closure size**: Total size of the store path and all its transitive dependencies.
* **Added size**:  Size of the store path, and all its _unique_ transitive
  dependencies. In other words, the cost of having that store path on top
  of all other paths. See [issue #14] for a better explanation.
