set -o xtrace
set -o errexit

tmpdir="$(mktemp -d)"
trap "rm -rf '$tmpdir'" EXIT

storePath=$(nix-build -A nixdu --no-out-link)

TMUX="tmux -S "$tmpdir/tmux.sock""
echo "$TMUX attach -r"
$TMUX new-session -d
sleep 5

$TMUX resize-window -x 200 -y 40
sleep 1
$TMUX send-keys "export PATH=$storePath/bin:\$PATH" ENTER
sleep 1
$TMUX send-keys "asciinema rec \"$tmpdir/demo.cast\"" ENTER
sleep 2
$TMUX send-keys "nixdu $storePath"
sleep 1
$TMUX send-keys Enter
sleep 2
$TMUX send-keys Right
sleep 1
$TMUX send-keys Down
sleep 1
$TMUX send-keys Right
sleep 1
$TMUX send-keys Down
sleep 1
$TMUX send-keys Down
sleep 1
$TMUX send-keys w
sleep 2
$TMUX send-keys Up
sleep 1
$TMUX send-keys Up
sleep 1
$TMUX send-keys Up
sleep 1
$TMUX send-keys Enter
sleep 1
$TMUX send-keys Left
sleep 5
$TMUX send-keys q
sleep 1
$TMUX send-keys 'exit' ENTER
sleep 1
$TMUX kill-session

asciinema upload "$tmpdir/demo.cast"
