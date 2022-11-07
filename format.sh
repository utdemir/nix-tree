#!/usr/bin/env bash

if [[ "$1" == "check" ]]; then
    ormolu_flags="--mode check"
    nixpkgs_fmt_flags="--check"
elif [[ "$1" == "format" ]]; then
    ormolu_flags="--mode inplace"
    nixpkgs_fmt_flags=""
else 
    echo "Usage: $0 [check|format]"
    exit 1
fi

set -o errexit
shopt -s globstar
set -o xtrace

ormolu $ormolu_flags {src,test}/**/*.hs
nixpkgs-fmt  $nixpkgs_fmt_flags default.nix flake.nix
