#!/usr/bin/env bash

set -o xtrace
set -o errexit

tmpdir="$(mktemp -d)"
trap "rm -rf '$tmpdir'" EXIT

nixTreePath=$(nix build --json | jq '.[].outputs.out' -r)
storePath=$(nix-build -E '(import <nixpkgs> {}).git')

TMUX="tmux -S "$tmpdir/tmux.sock""
echo "$TMUX attach -r"
$TMUX new-session -d
sleep 5

$TMUX resize-window -x 200 -y 40
sleep 1
$TMUX send-keys "export PATH=$nixTreePath/bin:\$PATH" ENTER
sleep 1
$TMUX send-keys "asciinema rec \"$tmpdir/demo.cast\"" ENTER
sleep 2
$TMUX send-keys "nix-tree  $storePath"
sleep 1
$TMUX send-keys Enter
sleep 2

# navigate to curl
for i in Right Down Down Down Down Down Right Down; do
  $TMUX send-keys $i
  sleep 0.5
done

$TMUX send-keys w
sleep 2

$TMUX send-keys Up
sleep 0.5
$TMUX send-keys Up
sleep 1

$TMUX send-keys Enter
sleep 1

$TMUX send-keys Left
sleep 4

$TMUX send-keys q
sleep 1
$TMUX send-keys 'nix-tree --help' ENTER
sleep 2

$TMUX send-keys 'exit' ENTER
sleep 1
$TMUX kill-session

asciinema upload "$tmpdir/demo.cast"
