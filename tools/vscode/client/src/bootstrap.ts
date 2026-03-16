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
		candidates.push(`/usr/local/bin/${binaryName}`, `/usr/bin/${binaryName}`);
	}
	return candidates;
}

function _findFirst(candidates: string[]): string | undefined {
	return candidates.find((c) => fs.existsSync(c));
}

async function _findBinary(
	configuredPath: string | undefined,
	defaultConfigValue: string,
	binaryName: string,
): Promise<string | undefined> {
	if (configuredPath && configuredPath !== defaultConfigValue) {
		if (fs.existsSync(configuredPath)) {
			return configuredPath;
		}
		vscode.window.showWarningMessage(
			`Musi: Configured ${binaryName} path does not exist: ${configuredPath}`,
		);
	}

	return (
		_findFirst(_getWorkspaceCandidatesFor(binaryName)) ??
		_findFirst(_getGlobalCandidatesFor(binaryName))
	);
}

async function _showBinaryNotFoundUI(opts: {
	message: string;
	cargoPackage: string;
	secondActionLabel: string;
	secondActionHandler: () => void;
}): Promise<void> {
	const action = await vscode.window.showErrorMessage(
		opts.message,
		"Open Terminal",
		opts.secondActionLabel,
	);

	if (action === "Open Terminal") {
		const terminal = vscode.window.createTerminal("Musi Build");
		terminal.sendText(`cargo build -p ${opts.cargoPackage}`);
		terminal.show();
	} else if (action === opts.secondActionLabel) {
		opts.secondActionHandler();
	}
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
	return _findBinary(
		config.lspPath,
		CONFIG_DEFAULTS.lspPath,
		getServerBinaryName(),
	);
}

/**
 * Display error dialog when server binary cannot be found.
 * Offers options to open terminal for building or view documentation.
 */
export async function showServerNotFoundUI() {
	return _showBinaryNotFoundUI({
		message:
			"Musi LSP server binary not found. Build with 'cargo build -p music_lsp'.",
		cargoPackage: "music_lsp",
		secondActionLabel: "Show Build Instructions",
		secondActionHandler: () =>
			vscode.env.openExternal(
				vscode.Uri.parse("https://github.com/musi-lang/musi#Installation"),
			),
	});
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
	return _findBinary(
		config.cliPath,
		CONFIG_DEFAULTS.cliPath,
		getCliBinaryName(),
	);
}

/**
 * Display error dialog when CLI binary cannot be found.
 * Offers options to open terminal for building or configure path.
 */
export async function showCliNotFoundUI() {
	return _showBinaryNotFoundUI({
		message:
			"Musi CLI binary not found. Build with 'cargo build -p musi' or configure musi.cliPath.",
		cargoPackage: "musi",
		secondActionLabel: "Open Settings",
		secondActionHandler: () =>
			vscode.commands.executeCommand(
				"workbench.action.openSettings",
				"musi.cliPath",
			),
	});
}
