#!/usr/bin/env sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
OUT="$ROOT/examples/24-software-rasterizer/assets"
mkdir -p "$OUT"

curl -fL \
  "https://raw.githubusercontent.com/McNopper/OpenGL/refs/heads/master/Binaries/monkey.obj" \
  -o "$OUT/monkey.obj"

printf 'wrote %s\n' "$OUT/monkey.obj"
