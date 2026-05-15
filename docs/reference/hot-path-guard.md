# Hot-Path Guard

This guard enforces benchmark budgets for dispatch-, allocation-, and clone-sensitive Musi VM hot paths.

## Targets

```sh
make bench-hot-path-guard
```

Refresh benchmark measurements before guarding:

```sh
python3 scripts/perf/hot_path_guard.py
```

Optional lint/check integration:

```sh
HOT_PATH_GUARD=1 make check
HOT_PATH_GUARD=1 make lint
```

## Guard Source

- Budget file: `docs/reference/hot-path-guard-budgets.json`
- Runner: `scripts/perf/hot_path_guard.py`
- Criterion data source: `target/criterion/**/new/estimates.json`

`make bench-hot-path-guard` reads existing Criterion outputs and enforces configured budgets.
`python3 scripts/perf/hot_path_guard.py` reruns bounded benchmark filters, refreshes outputs, and then enforces the same budgets.
