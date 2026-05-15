#!/usr/bin/env sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT"

for example in examples/[0-9]*; do
  [ -d "$example" ] || continue
  if [ "$example" = "examples/19-glfw-window" ] && [ "${MUSI_RUN_INTERACTIVE_EXAMPLES:-0}" != "1" ]; then
    printf '== %s (check; set MUSI_RUN_INTERACTIVE_EXAMPLES=1 to open the window)\n' "$example"
    command cargo run -q -p musi -- check "$example"
    continue
  fi
  if [ "$example" = "examples/24-software-rasterizer" ] && [ "${MUSI_RUN_INTERACTIVE_EXAMPLES:-0}" != "1" ]; then
    printf '== %s (check; set MUSI_RUN_INTERACTIVE_EXAMPLES=1 to open the window)\n' "$example"
    command cargo run -q -p musi -- check "$example"
    continue
  fi
  printf '== %s\n' "$example"
  command cargo run -q -p musi -- run "$example"
done
