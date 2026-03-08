import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import { getConfig } from "./config";
import {
	getCargoBinDir,
	getCliBinaryName,
	getServerBinaryName,
	isWindows,
} from "./utils";

function _exists(filePath: string): boolean {
	return fs.existsSync(filePath);
}

function _getWorkspaceCandidates(workspacePath: string): string[] {
	const binary = getServerBinaryName();
	return [
		path.join(workspacePath, "target", "debug", binary),
		path.join(workspacePath, "target", "release", binary),
	];
}

function _getGlobalCandidates(): string[] {
	const binary = getServerBinaryName();
	const candidates = [path.join(getCargoBinDir(), binary)];

	if (!isWindows()) {
		candidates.push(`/usr/local/bin/${binary}`, `/usr/bin/${binary}`);
	}

	return candidates;
}

function _showConfiguredPathWarning(configuredPath: string) {
	vscode.window.showWarningMessage(
		`Musi: Configured server path does not exist: ${configuredPath}`,
	);
}

/**
 * Locate `musi_lsp` server binary.
 *
 * Search order:
 * 1. User-configured `musi.server.path` setting
 * 2. Workspace `target/debug/musi_lsp` or `target/release/musi_lsp`
 * 3. Global paths: `~/.cargo/bin`, `/usr/local/bin`, `/usr/bin`
 *
 * @returns Absolute path to server binary, or `undefined` if not found.
 */
function _getUserConfiguredPath(): string | undefined {
	const config = getConfig();

	// Check musi.lspPath first (new setting), then fall back to musi.server.path
	const lspPath = config.lspPath !== "musi-lsp" ? config.lspPath : null;
	const candidatePath = lspPath ?? config.serverPath;

	if (candidatePath) {
		if (_exists(candidatePath)) {
			return candidatePath;
		}
		_showConfiguredPathWarning(candidatePath);
	}

	return undefined;
}

function _getWorkspacePath(): string | undefined {
	const workspacePath = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
	if (workspacePath) {
		for (const candidate of _getWorkspaceCandidates(workspacePath)) {
			if (_exists(candidate)) {
				return candidate;
			}
		}
	}

	return undefined;
}

function _getGlobalPath(): string | undefined {
	for (const candidate of _getGlobalCandidates()) {
		if (_exists(candidate)) {
			return candidate;
		}
	}

	return undefined;
}

export async function findServerPath(): Promise<string | undefined> {
	const userPath = _getUserConfiguredPath();
	if (userPath) {
		return userPath;
	}

	const workspacePath = _getWorkspacePath();
	if (workspacePath) {
		return workspacePath;
	}

	const globalPath = _getGlobalPath();
	if (globalPath) {
		return globalPath;
	}

	return undefined;
}

/**
 * Display error dialog when server binary cannot be found.
 * Offers options to open terminal for building or view documentation.
 */
export async function showServerNotFoundUI() {
	const action = await vscode.window.showErrorMessage(
		"Musi LSP server binary not found. Build with 'cargo build -p musi_lsp'.",
		"Open Terminal",
		"Show Build Instructions",
	);

	if (action === "Open Terminal") {
		const terminal = vscode.window.createTerminal("Musi Build");
		terminal.sendText("cargo build -p musi_lsp");
		terminal.show();
	} else if (action === "Show Build Instructions") {
		vscode.env.openExternal(
			vscode.Uri.parse("https://github.com/musi-lang/musi#Installation"),
		);
	}
}

// -- CLI Binary (musi) ---

function _getCliWorkspaceCandidates(workspacePath: string): string[] {
	const binary = getCliBinaryName();
	return [
		path.join(workspacePath, "target", "debug", binary),
		path.join(workspacePath, "target", "release", binary),
	];
}

function _getCliGlobalCandidates(): string[] {
	const binary = getCliBinaryName();
	const candidates = [path.join(getCargoBinDir(), binary)];

	if (!isWindows()) {
		candidates.push(`/usr/local/bin/${binary}`, `/usr/bin/${binary}`);
	}

	return candidates;
}

function _getCliUserConfiguredPath(): string | undefined {
	const config = getConfig();

	if (config.cliPath && config.cliPath !== "musi") {
		if (_exists(config.cliPath)) {
			return config.cliPath;
		}
		vscode.window.showWarningMessage(
			`Musi: Configured CLI path does not exist: ${config.cliPath}`,
		);
	}

	return undefined;
}

function _getCliWorkspacePath(): string | undefined {
	const workspacePath = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
	if (workspacePath) {
		for (const candidate of _getCliWorkspaceCandidates(workspacePath)) {
			if (_exists(candidate)) {
				return candidate;
			}
		}
	}

	return undefined;
}

function _getCliGlobalPath(): string | undefined {
	for (const candidate of _getCliGlobalCandidates()) {
		if (_exists(candidate)) {
			return candidate;
		}
	}

	return undefined;
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
	const userPath = _getCliUserConfiguredPath();
	if (userPath) {
		return userPath;
	}

	const workspacePath = _getCliWorkspacePath();
	if (workspacePath) {
		return workspacePath;
	}

	const globalPath = _getCliGlobalPath();
	if (globalPath) {
		return globalPath;
	}

	return undefined;
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
