#!/usr/bin/env sh

set -o xtrace
set -o errexit

fd -e hs -X ormolu --mode inplace {}
fd -e nix  -E nix/sources.nix -X nixpkgs-fmt {}
