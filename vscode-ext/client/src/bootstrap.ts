import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import { CONFIG_DEFAULTS, getConfig } from "./config.ts";
import { getCargoBinDir, getCliBinaryName, isWindows } from "./utils.ts";

function _getWorkspaceCandidatesFor(binaryName: string): string[] {
	const ws = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
	if (!ws) {
		return [];
	}
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

function _findBinary(
	configuredPath: string | undefined,
	defaultConfigValue: string,
	binaryName: string,
): string | undefined {
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
 * Locate `musi` CLI binary.
 *
 * Search order:
 * 1. User-configured `musi.cliPath` setting
 * 2. Workspace `target/debug/musi` or `target/release/musi`
 * 3. Global paths: `~/.cargo/bin`, `/usr/local/bin`, `/usr/bin`
 *
 * @returns Absolute path to CLI binary, or `undefined` if not found.
 */
export function findCliPath(): string | undefined {
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
export function showCliNotFoundUI() {
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
