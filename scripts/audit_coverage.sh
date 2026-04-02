#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: audit_coverage [--default] [options]
       audit_coverage <ext> <pattern> [options]
  --default:       Run the repo's built-in schema coverage audit suite
  -l, --lines:     Sort by highest LOC first (default: alphabetically by filename)
  -d, --dir DIR:   Search directory (default: current directory)
  -x, --exclude:   Exclude directories (can be used multiple times)
  -h, --help:      Show this help message

This lists files with extension <ext> that DO NOT match <pattern>, then reports LOC.
Pattern is a ripgrep regex (same semantics as `rg -L`).

Default suite:
  1. JSON files under config/ and plugins/ missing "$schema"
     (excluding plugin locale message files)
  2. JSON schema files under schemas/ missing "$id"
EOF
}

sort_by_loc=0
search_dir="."
exclude_dirs=()
default_suite=0

run_audit() {
  local ext="$1"
  local exception="$2"
  local label="$3"
  local audit_dir="$4"
  shift 4

  local -a audit_excludes=("$@")
  local -a rg_args=(
    --null
    --files-without-match
    "$exception"
    --glob
    "*.$ext"
  )
  local -a files=()
  local file

  if [[ ${#audit_excludes[@]} -gt 0 ]]; then
    for dir in "${audit_excludes[@]}"; do
      rg_args+=(--glob "!$dir/**")
    done
  fi

  while IFS= read -r -d '' file; do
    files+=("$file")
  done < <(rg "${rg_args[@]}" "$audit_dir")

  printf '== %s ==\n' "$label"
  if [[ ${#files[@]} -eq 0 ]]; then
    printf 'All matched.\n\n'
    return 0
  fi

  printf '%s\0' "${files[@]}" \
    | xargs -0 wc -l \
    | if [[ $sort_by_loc -eq 1 ]]; then
        sort -rn
      else
        sort -k2
      fi
  printf '\n'
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --default) default_suite=1; shift ;;
    -l|--lines) sort_by_loc=1; shift ;;
    -d|--dir) search_dir="$2"; shift 2 ;;
    -x|--exclude) exclude_dirs+=("$2"); shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) break ;;
  esac
done

if [[ $default_suite -eq 1 || $# -eq 0 ]]; then
  if [[ ${#exclude_dirs[@]} -gt 0 ]]; then
    run_audit "json" '"\$schema"' 'JSON files missing "$schema"' "$search_dir" \
      "schemas" "locales" "plugins/*/locales" "${exclude_dirs[@]}"
    run_audit "json" '"\$id"' 'Schema files missing "$id"' "$search_dir/schemas" \
      "${exclude_dirs[@]}"
  else
    run_audit "json" '"\$schema"' 'JSON files missing "$schema"' "$search_dir" \
      "schemas" "locales" "plugins/*/locales"
    run_audit "json" '"\$id"' 'Schema files missing "$id"' "$search_dir/schemas"
  fi
  exit 0
fi

if [[ $# -lt 2 ]]; then
  usage
  exit 1
fi

ext="$1"
exception="$2"
shift 2

# Makefile defaults need to escape leading `#` as `\#` to avoid comment parsing.
exception="${exception/\\#/#}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    -l|--lines) sort_by_loc=1; shift ;;
    -d|--dir) search_dir="$2"; shift 2 ;;
    -x|--exclude) exclude_dirs+=("$2"); shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) shift ;;
  esac
done

if [[ ${#exclude_dirs[@]} -gt 0 ]]; then
  run_audit "$ext" "$exception" "Files missing /$exception/" "$search_dir" "${exclude_dirs[@]}"
else
  run_audit "$ext" "$exception" "Files missing /$exception/" "$search_dir"
fi
