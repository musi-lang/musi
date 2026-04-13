import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import { findWorkspaceLspBinary } from "./binary-selection.ts";
import { CONFIG_DEFAULTS, getConfig } from "./config.ts";
import {
	getCargoBinDir,
	getCliBinaryName,
	getLspBinaryName,
	isWindows,
} from "./utils.ts";

function workspaceCandidates(binaryName: string): string[] {
	const workspace = workspacePath();
	if (!workspace) {
		return [];
	}
	return [
		path.join(workspace, "target", "debug", binaryName),
		path.join(workspace, "target", "release", binaryName),
	];
}

function globalCandidates(binaryName: string): string[] {
	const candidates = [path.join(getCargoBinDir(), binaryName)];
	const pathEntries = (process.env["PATH"] ?? "")
		.split(path.delimiter)
		.filter(Boolean)
		.map((entry) => path.join(entry, binaryName));

	if (!isWindows()) {
		candidates.push(`/usr/local/bin/${binaryName}`, `/usr/bin/${binaryName}`);
	}

	return [...candidates, ...pathEntries];
}

function firstExisting(candidates: readonly string[]): string | undefined {
	return candidates.find((candidate) => fs.existsSync(candidate));
}

function configuredBinary(
	configuredPath: string | undefined,
	defaultValue: string,
	displayName: string,
): string | undefined {
	if (!(configuredPath && configuredPath !== defaultValue)) {
		return undefined;
	}
	if (fs.existsSync(configuredPath)) {
		return configuredPath;
	}
	vscode.window
		.showWarningMessage(
			`Configured ${displayName} path does not exist: ${configuredPath}`,
		)
		.then(undefined, (error: unknown) => {
			console.error("[musi-vscode] warning message failed:", error);
		});
	return undefined;
}

function workspacePath(): string | undefined {
	return vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
}

export interface BinaryLookup {
	path?: string;
	staleWorkspacePath?: string;
	freshnessPath?: string;
}

function binaryLookup(
	path: string | undefined,
	staleWorkspacePath: string | undefined,
	freshnessPath: string | undefined,
): BinaryLookup {
	const lookup: BinaryLookup = {};
	if (path) {
		lookup.path = path;
	}
	if (staleWorkspacePath) {
		lookup.staleWorkspacePath = staleWorkspacePath;
	}
	if (freshnessPath) {
		lookup.freshnessPath = freshnessPath;
	}
	return lookup;
}

function findBinaryPath(
	configuredPath: string,
	defaultValue: string,
	binaryName: string,
	displayName: string,
): string | undefined {
	return (
		configuredBinary(configuredPath, defaultValue, displayName) ??
		firstExisting(workspaceCandidates(binaryName)) ??
		firstExisting(globalCandidates(binaryName))
	);
}

export function findCliPath(): string | undefined {
	const config = getConfig();
	return findBinaryPath(
		config.cliPath,
		CONFIG_DEFAULTS.cliPath,
		getCliBinaryName(),
		"Musi CLI",
	);
}

export function findLspBinary(): BinaryLookup {
	const config = getConfig();
	const configured = configuredBinary(
		config.lspPath,
		CONFIG_DEFAULTS.lspPath,
		"Musi LSP",
	);
	if (configured) {
		return binaryLookup(configured, undefined, undefined);
	}
	const workspaceBinary = findWorkspaceLspBinary(
		workspacePath(),
		getLspBinaryName(),
	);
	if (workspaceBinary.path) {
		return workspaceBinary;
	}
	const global = firstExisting(globalCandidates(getLspBinaryName()));
	if (global) {
		return binaryLookup(
			global,
			workspaceBinary.staleWorkspacePath,
			workspaceBinary.freshnessPath,
		);
	}
	return workspaceBinary;
}

export function findLspPath(): string | undefined {
	return findLspBinary().path;
}

export async function showCliNotFoundUI() {
	const action = await vscode.window.showErrorMessage(
		"Musi CLI binary not found. Configure musi.cliPath to an installed `musi` executable.",
		"Open Settings",
	);
	if (action === "Open Settings") {
		await vscode.commands.executeCommand(
			"workbench.action.openSettings",
			"musi.cliPath",
		);
	}
}

export async function showLspNotFoundUI() {
	const action = await vscode.window.showErrorMessage(
		"Musi LSP binary not found. Configure musi.lspPath to an installed `musi_lsp` executable.",
		"Open Settings",
	);
	if (action === "Open Settings") {
		await vscode.commands.executeCommand(
			"workbench.action.openSettings",
			"musi.lspPath",
		);
	}
}

export async function showStaleLspBinaryUI(
	staleWorkspacePath: string,
	freshnessPath?: string,
) {
	const freshnessDetail = freshnessPath
		? ` Source stamp: ${path.relative(workspacePath() ?? path.dirname(freshnessPath), freshnessPath)}.`
		: "";
	await vscode.window.showWarningMessage(
		`Workspace Musi LSP binary is stale: ${staleWorkspacePath}. Rebuild \`musi_lsp\` to restore current editor diagnostics.${freshnessDetail}`,
	);
}
