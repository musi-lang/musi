#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF' >&2
usage:
  bash tools/cargo_timeout.sh <seconds> -- <command...>

examples:
  bash tools/cargo_timeout.sh 5 -- cargo test -p music_syntax
  CARGO_BUILD_JOBS=1 bash tools/cargo_timeout.sh 15 -- cargo bench -p music_syntax --bench bench_lexer
EOF
  exit 2
}

if [[ $# -lt 3 ]]; then
  usage
fi

secs="$1"
shift

if [[ "${1:-}" != "--" ]]; then
  usage
fi
shift

if [[ ! "$secs" =~ ^[0-9]+$ ]]; then
  echo "error: <seconds> must be an integer, got: $secs" >&2
  exit 2
fi

if command -v timeout >/dev/null 2>&1; then
  exec timeout "${secs}s" "$@"
fi

if command -v gtimeout >/dev/null 2>&1; then
  exec gtimeout "${secs}s" "$@"
fi

if command -v perl >/dev/null 2>&1; then
  exec perl -e '
    my ($secs, @cmd) = @ARGV;
    alarm($secs);
    exec @cmd;
  ' "$secs" "$@"
fi

if command -v python3 >/dev/null 2>&1; then
  exec python3 -c '
import os, signal, subprocess, sys
secs = int(sys.argv[1])
cmd = sys.argv[2:]
proc = subprocess.Popen(cmd, start_new_session=True)
try:
    raise SystemExit(proc.wait(timeout=secs))
except subprocess.TimeoutExpired:
    os.killpg(proc.pid, signal.SIGTERM)
    raise SystemExit(124)
' "$secs" "$@"
fi

echo "error: no timeout implementation found (need timeout/gtimeout/perl/python3)." >&2
exit 127
