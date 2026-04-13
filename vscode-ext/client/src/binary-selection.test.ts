import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import test from "node:test";

import { findWorkspaceLspBinary } from "./binary-selection.ts";

function makeTempWorkspace(): string {
	return fs.mkdtempSync(path.join(os.tmpdir(), "musi-vscode-binary-"));
}

function writeFile(target: string): void {
	fs.mkdirSync(path.dirname(target), { recursive: true });
	fs.writeFileSync(target, "x");
}

function setMtime(target: string, timeMs: number): void {
	const time = new Date(timeMs);
	fs.utimesSync(target, time, time);
}

test("selects fresh workspace debug binary", () => {
	const workspace = makeTempWorkspace();
	const freshness = path.join(workspace, "crates/musi_foundation/src/lib.rs");
	const binary = path.join(workspace, "target/debug/musi_lsp");
	writeFile(freshness);
	writeFile(binary);
	setMtime(freshness, 1_000);
	setMtime(binary, 2_000);

	const selection = findWorkspaceLspBinary(workspace, "musi_lsp");

	assert.equal(selection.path, binary);
	assert.equal(selection.staleWorkspacePath, undefined);
	assert.equal(selection.freshnessPath, freshness);
	fs.rmSync(workspace, { recursive: true, force: true });
});

test("skips stale debug binary for fresh release binary", () => {
	const workspace = makeTempWorkspace();
	const freshness = path.join(workspace, "crates/musi_tooling/src/lib.rs");
	const staleDebug = path.join(workspace, "target/debug/musi_lsp");
	const freshRelease = path.join(workspace, "target/release/musi_lsp");
	writeFile(freshness);
	writeFile(staleDebug);
	writeFile(freshRelease);
	setMtime(staleDebug, 1_000);
	setMtime(freshness, 2_000);
	setMtime(freshRelease, 3_000);

	const selection = findWorkspaceLspBinary(workspace, "musi_lsp");

	assert.equal(selection.path, freshRelease);
	assert.equal(selection.staleWorkspacePath, staleDebug);
	assert.equal(selection.freshnessPath, freshness);
	fs.rmSync(workspace, { recursive: true, force: true });
});

test("reports stale workspace binary when no fresh candidate exists", () => {
	const workspace = makeTempWorkspace();
	const freshness = path.join(workspace, "Cargo.lock");
	const staleDebug = path.join(workspace, "target/debug/musi_lsp");
	writeFile(freshness);
	writeFile(staleDebug);
	setMtime(staleDebug, 1_000);
	setMtime(freshness, 2_000);

	const selection = findWorkspaceLspBinary(workspace, "musi_lsp");

	assert.equal(selection.path, undefined);
	assert.equal(selection.staleWorkspacePath, staleDebug);
	assert.equal(selection.freshnessPath, freshness);
	fs.rmSync(workspace, { recursive: true, force: true });
});
