#!/usr/bin/env bash
set -euo pipefail

root="${1:-crates}"
min_total="${2:-1500}"
max_share="${3:-0.75}"

if [[ ! -d "$root" ]]; then
  echo "missing $root/ directory" >&2
  exit 1
fi

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

while IFS= read -r file; do
  if [[ "$file" == */benches/* ]]; then
    continue
  fi
  if [[ "$(basename "$file")" == "tests.rs" ]]; then
    continue
  fi
  if rg -q "#\\[test\\]" "$file"; then
    continue
  fi

  lines="$(wc -l <"$file" | tr -d ' ')"
  crate="$(printf '%s\n' "$file" | awk -F/ '{print $2}')"
  printf '%s %s %s\n' "$crate" "$lines" "$file" >>"$tmp"
done < <(rg --files "$root" -g '*.rs' | sort)

if [[ ! -s "$tmp" ]]; then
  exit 0
fi

awk -v min_total="$min_total" -v max_share="$max_share" '
{
  crate = $1;
  lines = $2 + 0;
  file = $3;

  total[crate] += lines;
  if (lines > max_lines[crate] + 0) {
    max_lines[crate] = lines;
    max_file[crate] = file;
  }
}
END {
  violations = 0;
  print "== God-crate audit (largest-file share) ==";
  print "rules:";
  printf("  - total_loc >= %d\n", min_total);
  printf("  - largest_file_loc / total_loc >= %.2f\n\n", max_share);

  for (crate in total) {
    t = total[crate] + 0;
    m = max_lines[crate] + 0;
    share = (t > 0) ? (m / t) : 0;
    printf("  %s: total=%d max=%d share=%.3f file=%s\n", crate, t, m, share, max_file[crate]);
    if (t >= min_total && share >= max_share) {
      violations += 1;
    }
  }

  if (violations > 0) {
    print "";
    print "FAILED: crate dominated by a single file." > "/dev/stderr";
    exit 1;
  }
}
' "$tmp"

