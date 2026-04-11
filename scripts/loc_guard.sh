#!/usr/bin/env bash
set -euo pipefail

root="${1:-crates}"
limit="${2:-2000}"

if [[ ! -d "$root" ]]; then
  echo "missing $root/ directory" >&2
  exit 1
fi

violations="$(
  find "$root" -type f -name '*.rs' -print0 \
    | xargs -0 wc -l \
    | awk -v limit="$limit" '$2 != "total" && $1 > limit { printf("%s (%d lines)\n", $2, $1) }'
)"

if [[ -n "$violations" ]]; then
  echo "Rust LOC limit exceeded (limit=$limit):" >&2
  echo "$violations" >&2
  exit 1
fi
