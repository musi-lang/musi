#!/usr/bin/env python3

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[2]
CONFIG_PATH = ROOT / "docs/reference/hot-path-guard-budgets.json"
CRITERION_DIR = ROOT / "target/criterion"


@dataclass(frozen=True)
class HotPathBudget:
    benchmark_id: str
    run_filter: str
    max_ns: float
    sensitivity: str


def load_budgets(config_path: Path) -> list[HotPathBudget]:
    raw = json.loads(config_path.read_text(encoding="utf-8"))
    budgets: list[HotPathBudget] = []
    for item in raw["benchmarks"]:
        budgets.append(
            HotPathBudget(
                benchmark_id=item["benchmark_id"],
                run_filter=item["run_filter"],
                max_ns=float(item["max_ns"]),
                sensitivity=item["sensitivity"],
            )
        )
    return budgets


def run_bench_filter(toolchain: str, bench_filter: str) -> None:
    command = [
        "rustup",
        "run",
        toolchain,
        "cargo",
        "bench",
        "-p",
        "musi_vm",
        "--bench",
        "bench_vm",
        "--",
        bench_filter,
        "--noplot",
    ]
    print(f"[hot-path-guard] running: {' '.join(command)}")
    subprocess.run(command, cwd=ROOT, check=True)


def read_point_estimate_ns(benchmark_id: str) -> float:
    estimate_path = CRITERION_DIR / benchmark_id / "new/estimates.json"
    if not estimate_path.exists():
        raise FileNotFoundError(
            f"missing criterion estimate for `{benchmark_id}` at `{estimate_path}`"
        )
    data = json.loads(estimate_path.read_text(encoding="utf-8"))
    return float(data["mean"]["point_estimate"])


def print_results_table(rows: list[dict[str, Any]]) -> None:
    print(
        "benchmark_id | sensitivity | measured_ns | max_ns | headroom_ns | status",
    )
    print("--- | --- | ---: | ---: | ---: | ---")
    for row in rows:
        print(
            f"{row['benchmark_id']} | {row['sensitivity']} | "
            f"{row['measured_ns']:.3f} | {row['max_ns']:.3f} | "
            f"{row['headroom_ns']:.3f} | {row['status']}"
        )


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Run Musi VM hot-path benchmark guard and enforce budgets."
    )
    parser.add_argument(
        "--toolchain",
        default="1.95.0",
        help="rustup toolchain used for cargo bench (default: 1.95.0)",
    )
    parser.add_argument(
        "--skip-run",
        action="store_true",
        help="Only evaluate existing criterion output under target/criterion.",
    )
    parser.add_argument(
        "--config",
        default=str(CONFIG_PATH),
        help="Path to benchmark budget configuration JSON.",
    )
    args = parser.parse_args()

    budgets = load_budgets(Path(args.config))
    if not budgets:
        print("[hot-path-guard] no budgets configured")
        return 1

    if not args.skip_run:
        seen_filters: set[str] = set()
        for budget in budgets:
            if budget.run_filter in seen_filters:
                continue
            seen_filters.add(budget.run_filter)
            run_bench_filter(args.toolchain, budget.run_filter)

    rows: list[dict[str, Any]] = []
    failed = False
    for budget in budgets:
        try:
            measured_ns = read_point_estimate_ns(budget.benchmark_id)
        except FileNotFoundError as error:
            print(f"[hot-path-guard] {error}")
            failed = True
            continue
        headroom_ns = budget.max_ns - measured_ns
        status = "PASS" if headroom_ns >= 0 else "FAIL"
        if status == "FAIL":
            failed = True
        rows.append(
            {
                "benchmark_id": budget.benchmark_id,
                "sensitivity": budget.sensitivity,
                "measured_ns": measured_ns,
                "max_ns": budget.max_ns,
                "headroom_ns": headroom_ns,
                "status": status,
            }
        )

    if rows:
        print_results_table(rows)

    if failed:
        print("[hot-path-guard] budget regression detected")
        return 1

    print("[hot-path-guard] all guarded benchmarks are within budget")
    return 0


if __name__ == "__main__":
    sys.exit(main())
