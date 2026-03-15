import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import { CONFIG_DEFAULTS, getConfig } from "./config";
import {
	getCargoBinDir,
	getCliBinaryName,
	getServerBinaryName,
	isWindows,
} from "./utils";

function _exists(filePath: string): boolean {
	return fs.existsSync(filePath);
}

function _getWorkspaceCandidatesFor(binaryName: string): string[] {
	const ws = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
	if (!ws) return [];
	return [
		path.join(ws, "target", "debug", binaryName),
		path.join(ws, "target", "release", binaryName),
	];
}

function _getGlobalCandidatesFor(binaryName: string): string[] {
	const candidates = [path.join(getCargoBinDir(), binaryName)];
	if (!isWindows()) {
		candidates.push(
			`/usr/local/bin/${binaryName}`,
			`/usr/bin/${binaryName}`,
		);
	}
	return candidates;
}

function _findFirst(candidates: string[]): string | undefined {
	return candidates.find((c) => _exists(c));
}

/**
 * Locate `music_lsp` server binary.
 *
 * Search order:
 * 1. User-configured `musi.lspPath` setting
 * 2. Workspace `target/debug/music_lsp` or `target/release/music_lsp`
 * 3. Global paths: `~/.cargo/bin`, `/usr/local/bin`, `/usr/bin`
 *
 * @returns Absolute path to server binary, or `undefined` if not found.
 */
export async function findServerPath(): Promise<string | undefined> {
	const config = getConfig();

	if (config.lspPath !== CONFIG_DEFAULTS.lspPath) {
		if (_exists(config.lspPath)) {
			return config.lspPath;
		}
		vscode.window.showWarningMessage(
			`Musi: Configured server path does not exist: ${config.lspPath}`,
		);
	}

	const binary = getServerBinaryName();
	return (
		_findFirst(_getWorkspaceCandidatesFor(binary)) ??
		_findFirst(_getGlobalCandidatesFor(binary))
	);
}

/**
 * Display error dialog when server binary cannot be found.
 * Offers options to open terminal for building or view documentation.
 */
export async function showServerNotFoundUI() {
	const action = await vscode.window.showErrorMessage(
		"Musi LSP server binary not found. Build with 'cargo build -p music_lsp'.",
		"Open Terminal",
		"Show Build Instructions",
	);

	if (action === "Open Terminal") {
		const terminal = vscode.window.createTerminal("Musi Build");
		terminal.sendText("cargo build -p music_lsp");
		terminal.show();
	} else if (action === "Show Build Instructions") {
		vscode.env.openExternal(
			vscode.Uri.parse("https://github.com/musi-lang/musi#Installation"),
		);
	}
}

/**
 * Locate `musi` CLI binary.
 *
 * Search order:
 * 1. User-configured `musi.cliPath` setting
 * 2. Workspace `target/debug/musi` or `target/release/musi`
 * 3. Global paths: `~/.cargo/bin`, `/usr/local/bin`, `/usr/bin`
 *
 * @returns Absolute path to CLI binary, or `undefined` if not found.
 */
export async function findCliPath(): Promise<string | undefined> {
	const config = getConfig();

	if (config.cliPath && config.cliPath !== CONFIG_DEFAULTS.cliPath) {
		if (_exists(config.cliPath)) {
			return config.cliPath;
		}
		vscode.window.showWarningMessage(
			`Musi: Configured CLI path does not exist: ${config.cliPath}`,
		);
	}

	const binary = getCliBinaryName();
	return (
		_findFirst(_getWorkspaceCandidatesFor(binary)) ??
		_findFirst(_getGlobalCandidatesFor(binary))
	);
}

/**
 * Display error dialog when CLI binary cannot be found.
 * Offers options to open terminal for building or configure path.
 */
export async function showCliNotFoundUI() {
	const action = await vscode.window.showErrorMessage(
		"Musi CLI binary not found. Build with 'cargo build -p musi' or configure musi.cliPath.",
		"Open Terminal",
		"Open Settings",
	);

	if (action === "Open Terminal") {
		const terminal = vscode.window.createTerminal("Musi Build");
		terminal.sendText("cargo build -p musi");
		terminal.show();
	} else if (action === "Open Settings") {
		vscode.commands.executeCommand(
			"workbench.action.openSettings",
			"musi.cliPath",
		);
	}
}
