import * as fs from "node:fs";
import * as path from "node:path";

export interface BinarySelection {
	path?: string;
	staleWorkspacePath?: string;
	freshnessPath?: string;
}

const LSP_FRESHNESS_INPUTS = [
	"Cargo.lock",
	"Cargo.toml",
	"crates/musi_lsp/Cargo.toml",
	"crates/musi_lsp/src",
	"crates/musi_tooling/src",
	"crates/musi_project/src",
	"crates/musi_foundation/src",
] as const;

interface FileStamp {
	path: string;
	mtimeMs: number;
}

export function findWorkspaceLspBinary(
	workspace: string | undefined,
	binaryName: string,
): BinarySelection {
	if (!workspace) {
		return {};
	}
	const freshness = newestWorkspaceStamp(workspace, LSP_FRESHNESS_INPUTS);
	let staleWorkspacePath: string | undefined;
	for (const candidate of workspaceCandidates(workspace, binaryName)) {
		const candidateStamp = fileStamp(candidate);
		if (!candidateStamp) {
			continue;
		}
		if (freshness && candidateStamp.mtimeMs < freshness.mtimeMs) {
			staleWorkspacePath ??= candidate;
			continue;
		}
		return binarySelection(candidate, staleWorkspacePath, freshness?.path);
	}
	return binarySelection(undefined, staleWorkspacePath, freshness?.path);
}

function workspaceCandidates(workspace: string, binaryName: string): string[] {
	return [
		path.join(workspace, "target", "debug", binaryName),
		path.join(workspace, "target", "release", binaryName),
	];
}

function newestWorkspaceStamp(
	workspace: string,
	inputs: readonly string[],
): FileStamp | undefined {
	let newest: FileStamp | undefined;
	for (const input of inputs) {
		newest = newerStamp(newest, newestPathStamp(path.join(workspace, input)));
	}
	return newest;
}

function newestPathStamp(target: string): FileStamp | undefined {
	const stat = safeStat(target);
	if (!stat) {
		return undefined;
	}
	if (!stat.isDirectory()) {
		return { path: target, mtimeMs: stat.mtimeMs };
	}
	let newest: FileStamp | undefined;
	for (const entry of fs.readdirSync(target, { withFileTypes: true })) {
		newest = newerStamp(newest, newestPathStamp(path.join(target, entry.name)));
	}
	return newest;
}

function newerStamp(
	current: FileStamp | undefined,
	next: FileStamp | undefined,
): FileStamp | undefined {
	if (!next) {
		return current;
	}
	if (!current || next.mtimeMs > current.mtimeMs) {
		return next;
	}
	return current;
}

function fileStamp(target: string): FileStamp | undefined {
	const stat = safeStat(target);
	if (!stat || stat.isDirectory()) {
		return undefined;
	}
	return { path: target, mtimeMs: stat.mtimeMs };
}

function safeStat(target: string): fs.Stats | undefined {
	try {
		return fs.statSync(target, { throwIfNoEntry: false });
	} catch {
		return undefined;
	}
}

function binarySelection(
	path: string | undefined,
	staleWorkspacePath: string | undefined,
	freshnessPath: string | undefined,
): BinarySelection {
	const selection: BinarySelection = {};
	if (path) {
		selection.path = path;
	}
	if (staleWorkspacePath) {
		selection.staleWorkspacePath = staleWorkspacePath;
	}
	if (freshnessPath) {
		selection.freshnessPath = freshnessPath;
	}
	return selection;
}
