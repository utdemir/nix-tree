#!/usr/bin/env sh

set -o xtrace
set -o errexit

fd -e hs -X ormolu -o -XTypeApplications --mode inplace {}
nixpkgs-fmt flake.nix
