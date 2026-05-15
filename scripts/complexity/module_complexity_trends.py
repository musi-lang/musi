#!/usr/bin/env python3

from __future__ import annotations

import argparse
import csv
import json
import subprocess
import sys
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[2]
BUDGETS_PATH = ROOT / "docs/reference/module-complexity-budgets.json"
HISTORY_PATH = ROOT / "docs/reference/module-complexity-trends.csv"
REPORT_PATH = ROOT / "docs/reference/module-complexity-trends.md"


@dataclass(frozen=True)
class ModuleBudget:
    owner: str
    path: str
    max_cyclomatic_sum: int
    max_cyclomatic_max_fn: int
    baseline_cyclomatic_sum: int
    baseline_cyclomatic_max_fn: int
    max_delta_sum_from_baseline: int
    max_delta_max_fn_from_baseline: int


def load_budgets(path: Path) -> list[ModuleBudget]:
    raw = json.loads(path.read_text(encoding="utf-8"))
    modules: list[ModuleBudget] = []
    for module in raw["modules"]:
        modules.append(
            ModuleBudget(
                owner=module["owner"],
                path=module["path"],
                max_cyclomatic_sum=int(module["max_cyclomatic_sum"]),
                max_cyclomatic_max_fn=int(module["max_cyclomatic_max_fn"]),
                baseline_cyclomatic_sum=int(module["baseline_cyclomatic_sum"]),
                baseline_cyclomatic_max_fn=int(module["baseline_cyclomatic_max_fn"]),
                max_delta_sum_from_baseline=int(
                    module["max_delta_sum_from_baseline"]
                ),
                max_delta_max_fn_from_baseline=int(
                    module["max_delta_max_fn_from_baseline"]
                ),
            )
        )
    return modules


def run_rscheck_json(output_path: Path) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    command = ["rscheck", "check", "--format", "json", "--output", str(output_path)]
    print(f"[complexity-trend] running: {' '.join(command)}")
    result = subprocess.run(command, cwd=ROOT, check=False)
    if result.returncode > 1:
        raise RuntimeError("rscheck failed with non-recoverable exit status")


def load_metrics(rscheck_json_path: Path) -> dict[str, dict[str, int]]:
    raw = json.loads(rscheck_json_path.read_text(encoding="utf-8"))
    per_file = raw.get("metrics", {}).get("per_file", [])
    metrics: dict[str, dict[str, int]] = {}
    for metric in per_file:
        rel_path = Path(metric["path"]).resolve().relative_to(ROOT).as_posix()
        metrics[rel_path] = {
            "cyclomatic_sum": int(metric["cyclomatic_sum"]),
            "cyclomatic_max_fn": int(metric["cyclomatic_max_fn"]),
        }
    return metrics


def latest_history_by_path(history_path: Path) -> dict[str, dict[str, Any]]:
    if not history_path.exists():
        return {}
    latest: dict[str, dict[str, Any]] = {}
    with history_path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            latest[row["path"]] = row
    return latest


def print_guard_table(rows: list[dict[str, Any]]) -> None:
    print(
        "owner | path | sum | max_fn | baseline_sum | baseline_max_fn | "
        "delta_sum | delta_max_fn | status"
    )
    print("--- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---")
    for row in rows:
        print(
            f"{row['owner']} | {row['path']} | {row['sum']} | {row['max_fn']} | "
            f"{row['baseline_sum']} | {row['baseline_max_fn']} | "
            f"{row['delta_sum']} | {row['delta_max_fn']} | {row['status']}"
        )


def build_rows(
    budgets: list[ModuleBudget],
    metrics: dict[str, dict[str, int]],
    history: dict[str, dict[str, Any]],
) -> list[dict[str, Any]]:
    rows: list[dict[str, Any]] = []
    for budget in budgets:
        if budget.path not in metrics:
            raise RuntimeError(f"module metric missing for `{budget.path}`")

        current = metrics[budget.path]
        delta_sum = current["cyclomatic_sum"] - budget.baseline_cyclomatic_sum
        delta_max_fn = (
            current["cyclomatic_max_fn"] - budget.baseline_cyclomatic_max_fn
        )
        status = "PASS"
        if current["cyclomatic_sum"] > budget.max_cyclomatic_sum:
            status = "FAIL"
        if current["cyclomatic_max_fn"] > budget.max_cyclomatic_max_fn:
            status = "FAIL"
        if delta_sum > budget.max_delta_sum_from_baseline:
            status = "FAIL"
        if delta_max_fn > budget.max_delta_max_fn_from_baseline:
            status = "FAIL"

        previous = history.get(budget.path)
        previous_sum = int(previous["cyclomatic_sum"]) if previous else None
        previous_max_fn = int(previous["cyclomatic_max_fn"]) if previous else None

        rows.append(
            {
                "owner": budget.owner,
                "path": budget.path,
                "sum": current["cyclomatic_sum"],
                "max_fn": current["cyclomatic_max_fn"],
                "baseline_sum": budget.baseline_cyclomatic_sum,
                "baseline_max_fn": budget.baseline_cyclomatic_max_fn,
                "delta_sum": delta_sum,
                "delta_max_fn": delta_max_fn,
                "status": status,
                "previous_sum": previous_sum,
                "previous_max_fn": previous_max_fn,
            }
        )
    return rows


def append_history(history_path: Path, rows: list[dict[str, Any]]) -> None:
    history_path.parent.mkdir(parents=True, exist_ok=True)
    write_header = not history_path.exists()
    timestamp = datetime.now(tz=UTC).replace(microsecond=0).isoformat()
    commit = (
        subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"],
            cwd=ROOT,
            check=True,
            capture_output=True,
            text=True,
        )
        .stdout.strip()
    )
    with history_path.open("a", encoding="utf-8", newline="") as handle:
        fieldnames = [
            "timestamp_utc",
            "commit",
            "owner",
            "path",
            "cyclomatic_sum",
            "cyclomatic_max_fn",
        ]
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        if write_header:
            writer.writeheader()
        for row in rows:
            writer.writerow(
                {
                    "timestamp_utc": timestamp,
                    "commit": commit,
                    "owner": row["owner"],
                    "path": row["path"],
                    "cyclomatic_sum": row["sum"],
                    "cyclomatic_max_fn": row["max_fn"],
                }
            )


def write_report(report_path: Path, rows: list[dict[str, Any]]) -> None:
    report_path.parent.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now(tz=UTC).replace(microsecond=0).isoformat()
    lines = [
        "# Module Complexity Trends",
        "",
        f"Generated at `{timestamp}` from `rscheck` `metrics.per_file` cyclomatic output.",
        "",
        "| Owner | Module | Cyclomatic Sum | Max Function | Baseline Sum | Baseline Max Function | Delta Sum | Delta Max Function | Status |",
        "| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- |",
    ]
    for row in rows:
        lines.append(
            f"| {row['owner']} | `{row['path']}` | {row['sum']} | {row['max_fn']} | "
            f"{row['baseline_sum']} | {row['baseline_max_fn']} | {row['delta_sum']} | "
            f"{row['delta_max_fn']} | {row['status']} |"
        )
    lines.extend(
        [
            "",
            "## Guard Rules",
            "",
            "- `cyclomatic_sum` must stay below each module budget.",
            "- `cyclomatic_max_fn` must stay below each module budget.",
            "- Baseline deltas must stay below each module delta budget.",
        ]
    )
    report_path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Guard and report per-module rscheck complexity trends."
    )
    parser.add_argument(
        "--mode",
        choices=["guard", "report"],
        required=True,
        help="guard: enforce budgets, report: enforce and emit report/history outputs",
    )
    parser.add_argument(
        "--rscheck-json",
        default="target/rscheck/latest.json",
        help="Path to rscheck JSON output",
    )
    parser.add_argument(
        "--run-rscheck",
        action="store_true",
        help="Run rscheck JSON export before reading metrics",
    )
    parser.add_argument(
        "--write-report",
        action="store_true",
        help="Write markdown trend report (defaults to true in report mode)",
    )
    parser.add_argument(
        "--append-history",
        action="store_true",
        help="Append current metrics to the CSV history (report mode only)",
    )
    args = parser.parse_args()

    rscheck_json_path = (ROOT / args.rscheck_json).resolve()
    if args.run_rscheck or not rscheck_json_path.exists():
        run_rscheck_json(rscheck_json_path)

    budgets = load_budgets(BUDGETS_PATH)
    metrics = load_metrics(rscheck_json_path)
    history = latest_history_by_path(HISTORY_PATH)
    rows = build_rows(budgets, metrics, history)
    print_guard_table(rows)

    has_failures = any(row["status"] == "FAIL" for row in rows)

    should_write_report = args.mode == "report" or args.write_report
    if should_write_report:
        write_report(REPORT_PATH, rows)

    if args.mode == "report" and args.append_history:
        append_history(HISTORY_PATH, rows)

    if has_failures:
        print("[complexity-trend] complexity budget regression detected")
        return 1

    print("[complexity-trend] all tracked modules are within budget")
    return 0


if __name__ == "__main__":
    sys.exit(main())
