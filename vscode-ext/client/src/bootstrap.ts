import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import { CONFIG_DEFAULTS, getConfig } from "./config.ts";
import {
	getCargoBinDir,
	getCliBinaryName,
	getLspBinaryName,
	isWindows,
} from "./utils.ts";

function workspaceCandidates(binaryName: string): string[] {
	const workspace = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
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

export function findLspPath(): string | undefined {
	const config = getConfig();
	return findBinaryPath(
		config.lspPath,
		CONFIG_DEFAULTS.lspPath,
		getLspBinaryName(),
		"Musi LSP",
	);
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
