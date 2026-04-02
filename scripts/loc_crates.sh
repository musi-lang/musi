#!/usr/bin/env bash
set -euo pipefail

root="${1:-crates}"

if [[ ! -d "$root" ]]; then
  echo "missing $root/ directory" >&2
  exit 1
fi

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

while IFS= read -r file; do
  lines="$(wc -l <"$file" | tr -d ' ')"
  crate="$(printf '%s\n' "$file" | awk -F/ '{print $2}')"
  is_tests_rs=0
  if [[ "$(basename "$file")" == "tests.rs" ]]; then
    is_tests_rs=1
  fi
  has_test_attr=0
  if rg -q "#\\[test\\]" "$file"; then
    has_test_attr=1
  fi
  printf '%s %s %s %s\n' "$crate" "$lines" "$is_tests_rs" "$has_test_attr" >>"$tmp"
done < <(rg --files --no-ignore --hidden "$root" -g '*.rs' | sort)

awk '
{
  crate = $1;
  lines = $2 + 0;
  is_tests_rs = $3 + 0;
  has_test_attr = $4 + 0;

  total_all += lines;
  all[crate] += lines;

  if (is_tests_rs == 0 && has_test_attr == 0) {
    total_no_tests += lines;
    no_tests[crate] += lines;
  }
}
END {
  printf("%s Rust LOC (all .rs files): %d\n", "'"$root/"'", total_all);
  printf("%s Rust LOC (excluding tests.rs + files containing #[test]): %d\n", "'"$root/"'", total_no_tests);
  print "";
  print "Per crate:";
  for (crate in all) {
    printf("  %s: all=%d, no_tests=%d\n", crate, all[crate], no_tests[crate] + 0);
  }
}
' "$tmp"
