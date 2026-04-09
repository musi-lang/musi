#!/usr/bin/env bash
set -euo pipefail

root="${1:-crates_new}"

if [[ ! -d "$root" ]]; then
  echo "missing $root/ directory" >&2
  exit 1
fi

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

files=()
while IFS= read -r file; do
  files+=("$file")
done < <(
  rg --files "$root" \
    --glob '**/*.rs' \
    --glob '!**/tests.rs' \
    --glob '!**/benches/**'
)

if [[ ${#files[@]} -eq 0 ]]; then
  exit 0
fi

perl -ne '
  next if /^\s*(?:use|pub use)\b/;
  next if /^\s*(?:(?:\/\/\/?)|\/\*|\*|\*\/)/;

  while (/(?<![\w:])((?:music_[a-z_]+::|crate::[A-Za-z_][A-Za-z0-9_]*::)[A-Za-z_][A-Za-z0-9_:]*)/g) {
    print "$ARGV:$.:$1\n";
  }
' "${files[@]}" | sort -u > "$tmp"

if [[ -s "$tmp" ]]; then
  echo 'Forbidden fully qualified paths found outside imports:' >&2
  cat "$tmp" >&2
  exit 1
fi
