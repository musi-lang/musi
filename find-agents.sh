#!/usr/bin/env bash

STAR_THRESHOLD=5000
PER_PAGE=100
PAGES=5

API="https://api.github.com"
AUTH_HEADER="Authorization: token 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"

for ((page=1; page<=PAGES; page++)); do
  curl -s \
    -H "$AUTH_HEADER" \
    "$API/search/repositories?q=stars:>$STAR_THRESHOLD&sort=stars&order=desc&per_page=$PER_PAGE&page=$page" |
  jq -r '.items[].full_name' |
  while read -r repo; do
    status=$(curl -s -o /dev/null -w "%{http_code}" \
      -H "$AUTH_HEADER" \
      "$API/repos/$repo/contents/AGENTS.md")

    if [ "$status" = "200" ]; then
      echo "$repo"
    fi
  done
done
EOF
